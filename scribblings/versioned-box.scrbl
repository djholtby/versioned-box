#lang scribble/doc

@(require scribble/manual
          (for-label versioned-box))
@(define paper-url "http://hdl.handle.net/1802/2101")

@title{versioned-box: MVCC in Racket}
@author[(author+email "Dan Holtby" "djholtby@uwaterloo.ca")]

@defmodule[versioned-box]

versioned-box is a library that provides Software Transactional Memory (STM) by way of Multi-Versioned Concurrency Control (MVCC).
The ideas are based largely on @hyperlink[paper-url]{@italic{Versioned Boxes as the Basis for Memory Transactions}}, though currently not all features are implemented.

@defproc[(make-vbox [value any/c])
         vbox?]{Returns a new versioned box that contains @racket[value]}

@defproc[(vbox-ref [vb vbox?]) any/c]{Returns the value contained in @racket[vb].  If called during a transaction, returns the value that @racket[vb] contained when the transaction was started,
                                              and adds @racket[vb] to the transaction read set.}

@defproc[(vbox-set! [vb vbox?]
                    [v any/c])
         void?]{If called outside a transaction, increments the timestamp and adds @racket[v] as the most recent version of @racket[vb]'s value.

                   If called inside a transaction, sets the transaction-local value of @racket[vb] to @racket[v], adding @racket[vb] to the transaction's write set.
                   @racket[vb]'s state is only updated if the transaction successfully commits.}

@defproc[(vbox-cas! [vb vbox?]
                    [update (-> any/c any/c)])
         void?]{If called outside a transaction, applies @racket[update] to @racket[vb]'s value and then updates @racket[vb] to be the value returned by @racket[update].  If @racket[vb] has been updated since the initial read, the process repeats until a consistent write can be achieved.  (For this reason it is important to avoid all side effects within the update proc, or they may be repeated).

                   If called within a transaction, is equivalent to @racket[(vbox-set! (update (vbox-ref vb)))]}
                  


@defform*[
          ((with-transaction  expr ...)
           (with-transaction #:mode mode-hint))
         #:grammar
         [(mode-hint (any/c 'read 'write 'read/write 'read:restart))]
         ]{Evaluates the exprs in order, and ignores the results of all but the last expr.
                                all access to vbox values are recorded in a transaction.  When the last expr has been evaluated, the transaction is either committed (if no vbox contents have been updated since the transaction began) or rolled back, in which case the transaction repeats.
                                
                                If a continuation breaks out of the with-transaction syntax, the current transaction is automatically rolled back (and in this case, will not be restarted).

                                Optionally, a mode hint can be provided.  Transactions that do not write to any vboxes can be run in 'read mode, which is more efficient as it does not need
                                to keep track of vbox access (which requires two hash tables).  Transactions that do not read from any vboxes can be run in 'write mode, which is also more efficient
                                for the same reason.  Attempts to write during 'read mode or read during 'write mode result in the transaction failing and being restarted in 'read/write mode.

                                If no hint is provided then the default assumption is a 'read/write transaction.

                                Versioned boxes also support 'read:restart mode.  These special read-only transactions record the value they returned.
                                If the parent transaction is inconsistent only because of the boxes read 'read:restart transaction, then the 'read:restar transaction is run again, and
                                if the return value is unchanged, the parent transaction is considered consistent anyway.  The restarted transaction is executed in atomic mode, so this should be
                                reserved for relatively fast transactions only.
                                }
