#lang scribble/manual

@(require scribble/manual
          (for-label versioned-box))
@(define paper-url "http://hdl.handle.net/1802/2101")

@title{versioned-box: MVCC in Racket}
@author[(author+email "Dan Holtby" "djholtby@uwaterloo.ca")]

@defmodule[versioned-box]

versioned-box is a library that provides Software Transactional Memory (STM) by way of Multi-Versioned Concurrency Control (MVCC).
The ideas are based largely on @hyperlink[paper-url]{@italic{Versioned Boxes as the Basis for Memory Transactions}}, though currently not all features are implemented.

set-transaction-counter! get-transaction-timestamp

@defproc[(get-current-timestamp) exact-nonnegative-integer?]{Returns the time the current transaction was started, or the current time
 if no transaction is currently active.}


@defproc[(set-transaction-counter! [count exact-nonnegative-integer?]) void?]{Resets the current transaction counter to @racket[count].  Raises
 an error if count is less than the current timestamp (rewinding time would break running transactions and the vbox garbage collector).

 This is intended to be called before any vboxes have been created.}

@defproc[(transaction-mode? [v any/c]) boolean?]{Produces @racket[#t] if v is a valid transaction mode (@racket['read] for read-only, @racket['write] for write-only, @racket['read/write] for the default read/write mode,
 and @racket['read:restart] for a restartable read-only method).}

@defproc[(vbox? [value any/c]) boolean?]{Returns true if value is a versioned box, false otherwise}

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
 because there is only one value in the read and write sets, and the simplified logical flow.

 Note that update should not have side effects, since it may potentially be called more than once before the transaction is committed.
 Unlike with a regular transasction, @racket[defer] cannot be used to defer side effects until commit time.  
 
}
                  
@defproc[(call-with-mvcc-transaction [thunk (-> X)]
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
  (with-transaction #:mode mode-hint expr ...))
 #:grammar
 [(mode-hint transaction-mode?)]
 ]{
 Syntax equivalent to @racket[(call-with-mvcc-transaction (lambda () expr ...))] or
 @racket[(call-with-mvcc-transaction (lambda () expr ...) #:mode mode-hint)] respectively.  
}

@defproc[(defer [proc (-> X ... any/c)]
           [arg X] ...) void?]{
 Defers a call to @racket[proc] with the given arguments @racket[X ...] until the current top-level transaction has been comitted.
 Defered procs are called in the order that they were defered in, and must not read from or write to vboxes,
 or start a transaction of their own.  

 Called outside of a transaction, immediately applies proc to the supplied arguments.  
}

@defproc[(finalize [proc (-> X ... any/c)]
                   [art X] ...) void?]{Defers a call to @racket[proc] with the given arguments @racket[X ...] until the current top-level transaction has been comitted.
 Finalize procs are called in atomic mode.  They must not start new tranactions, but are allowed to
 read from and write to vboxes.

 Since they are run in atomic mode, they will always be consistent.  A finalizer for a read-only transaction
 IS able to write to vboxes.  

}
