#lang scribble/doc

@(require scribble/manual
          (for-label versioned-box))
@(define paper-url "http://hdl.handle.net/1802/2101")

@title{versioned-box: MVCC in Racket}
@author[(author+email "Dan Holtby" "djholtby@uwaterloo.ca")]

@defmodule[versioned-box]

versioned-box is a library that provides Software Transactional Memory (STM) by way of Multi-Versioned Concurrency Control (MVCC).
The ideas are based largely on @hyperlink[paper-url]{@italic{Versioned Boxes as the Basis for Memory Transactions}}, though currently not all features are implemented.

@defproc[(transaction-mode? [v any/c]) boolean?]{Produces @racket[#t] if v is a valid transaction mode (@racket['read] for read-only, @racket['write] for write-only, @racket['read/write] for the default read/write mode,
                                                                                                 and @racket['read:restart] for a restartable read-only method).}

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
         void?]{If called within a transaction, is equivalent to @racket[(vbox-set! (update (vbox-ref vb)))]

 If called outside a transaction, is mostly equivalent to the above within a read/write transaction, but much more efficient,
 because there is only one value in the read and write sets, and the the simplified logical flow.  

 Note that update should not have side effects, since it may potentially be called more than once before the transaction is committed.
However, because of the simplified structure, @racket[defer] cannot be used to defer side effects until commit time.  
 
                   }
                  
@defproc[(call-with-transaction [thunk (-> X)]
                                [#:mode mode [transaction-mode? 'read/write]])
         X]{Evaluates thunk within the context of transaction.  All vbox reads return the value those boxes had immediately before thunk was
 first called, and vbox writes are made to temporary storage until the transactoin is committed.

 When the thunk returns, the transaction is checked for consistenecy and committed.
 If the transaction is not consistent (it is a read/write transaction, and one or more of the reads was invalidated while thunk
 was running) then the transaction is rolled back and thunk is repeated until it can be committed successfully.
 
 Because thunk can potentially be repeated many times, all side effects should be avoided, or placed in a finalizer (see @racket[defer])
 
 Any continuation jumps out of thunk result in rolling back the transaction, and in this case the transaction is fully aborted
 (it will not be repeated).  A continuation barrier (TODO MAKE A COOL LINK) prevents jumps back into thunk.
 
 If mode is set to read, or write, then less bookkeeping is required and the transaction will be more efficient.
 read and write are considered to be hints only.  If a write occurs during a read-only transaction,
 or a read occurs during a write-only transaction, the transaction will
 be restarted in read/write mode (it needs to be restarted as necessary bookkeeping was not being done).
 
 There is an additional mode, read:restart, intended to be for restartable methods.  When a restartable method is started
 in a nested context, it keeps track of its read seperately, and also keeps track of its final return value(s).
 
 If the top-level transaction is consistent other than the reads from a restartable method, then that restartable method is run again
 in atomic mode, and the conflict is ignored if this repeat call produces the same value (as far as @racket[equal?] is concerned).

 TODO: option to have restartable methods specify their match predicate?
             
}

@defform*[
          ((with-transaction  expr ...)
           (with-transaction #:mode mode-hint))
         #:grammar
         [(mode-hint transaction-mode?)]
         ]{
            Syntax equivalent to @racket[(call-with-transaction (lambda () expr ...) #:mode mode-hint)].  
          }

@defproc[(defer [proc (-> X ... any/c)]
                [arg X] ...) void?]{
                                    Defers a call to thunk with the given arguments until the current top-level transaction has been comitted.
                                    (proc is called outside of the transaction so it must not read from or write to vboxes).

                                    Called outside of a transaction, immediately applies proc to the supplied arguments.
                                    }
