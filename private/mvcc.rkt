#lang racket/base

(require ffi/unsafe/atomic (for-syntax racket/base syntax/parse) racket/stxparam racket/set racket/list "heap.rkt" "queue.rkt")
(provide exn:fail:transaction? exn:fail:transaction:not-in-transaction? completed-write-transactions)
(provide make-vbox vbox-ref vbox-set! vbox-cas! transaction-rollback with-transaction call-with-transaction transaction-mode?)

;(define atomic-semaphore (make-semaphore 1))
;(define (start-atomic) (semaphore-wait atomic-semaphore))
;(define (end-atomic) (semaphore-post atomic-semaphore))

;(define start-atomic void)
;(define end-atomic void)

(define restart-count 0)
(provide restart-count)

(struct vbox ([body #:mutable])
  #:methods gen:custom-write
  [(define (write-proc vb port mode)
     (define value (vbox-ref vb))
     (case mode
       [(#t) (display "#*" port) (write value port)]
       [(#f) (display value port)]
       [else (display "(make-vbox " port)
             (print value port mode)
             (display ")")]))])



(struct vbody (id value [next #:mutable]) #:transparent)
(struct transaction-stack ([id #:mutable] [count #:mutable] [length #:mutable] read-set [write-tables #:mutable] [abort #:mutable] [restart #:mutable] semaphore
                                          [modes #:mutable] [record #:mutable] [queues #:mutable] [in-restartable? #:mutable] restartable-queue [state #:mutable] [tag #:mutable]) #:transparent) 
(struct transaction-record (id [writes #:mutable] semaphore tag) #:transparent)
(struct restartable-method-record (read-set [return #:mutable] proc))

(define initial-size 4)
(struct exn:fail:transaction exn:fail () #:transparent #:extra-constructor-name make-exn:fail:transaction)
(struct exn:fail:transaction:not-in-transaction exn:fail:transaction () #:transparent #:extra-constructor-name make-exn:fail:transaction:not-in-transaction)

(define (raise-transaction-error who message)
  (raise (make-exn:fail:transaction (format "~a: ~a" who message) (current-continuation-marks))))

(define (raise-not-in-transaction-error who message)
  (raise (make-exn:fail:transaction:not-in-transaction (format "~a: ~a" who message) (current-continuation-marks))))




(define current-transaction (make-thread-cell #f))

;; TransactionState is one of 'none 'transaction 'commit 'restart
;; none - no active transaction
;; transaction - one or more active transactions
;; commit - in the process of committing a transaction (this will occur during the execution of any defered procedure calls)
;; restart - in atomic mode to retry a restartable method


;; this is the timestamp
(define completed-write-transactions 0)


(define (transaction-mode? v)
  (and (memq v '(read write read/write read:restart)) #t))

;; whether it's allowed to start child transaction nested in parent transaction.
;; (basically, you shouldn't be starting a read-only within a write-only and vise versa
;; compastible-modes?: TMode TMode -> Bool

(define (compatible-modes? parent child)
  (or (eq? parent 'read/write)
      (eq? child parent)
      (and (eq? parent 'read) (eq? child 'read:restart))))

;; what the top-level transaction mode is for transaction stack t
(define (transaction-stack-mode t)
  (vector-ref (transaction-stack-modes t) 0))

;; the set of vboxes that have been read by the currently active restartable method
;;   (or the most recently finished one, but you shouldn't be accessing this outside the context of an active method)

(define (restartable-read-set t)
  (restartable-method-record-read-set (queue-peek-back (transaction-stack-restartable-queue t))))

;; Picks the appropriate set for adding / checking which vboxes have been read by the current transaction / method

(define (transaction-reads t)
  (if (transaction-stack-in-restartable? t)
      (restartable-read-set t)
      (transaction-stack-read-set t)))

;; set up the thread-specific transaction data structures

(define (init-transaction-data)
  (thread-cell-set! current-transaction
                    (transaction-stack 0 ; timestamp when top level transaction was opened
                                       0 ; how many nested transactions are open
                                       initial-size ; length of vectors
                                       (mutable-seteq) ; read-set
                                       (build-vector initial-size (lambda (i) (make-hasheq))) ; write-table stack
                                       (make-vector initial-size #f) ; abort continuation stack
                                       #f ; restart continuation
                                       (make-semaphore 0) ; semaphore (signal to let GC know it can reap unreachable histories)
                                       (make-vector initial-size #f) ; mode stack
                                       #f  ; transaction record
                                       (build-vector initial-size (lambda (i) (make-queue))) ; deferred thunk queues
                                       #f ; currently nested within a restartable method?
                                       (make-queue) ; restartable method queue
                                       'none ; state
                                       #f    ; tag
                                       )))

(provide set-transaction-tag!)

;; sets the nametag for the current thread (useful for debugging transaction issues)

(define (set-transaction-tag! t)
  (unless (thread-cell-ref current-transaction)
    (init-transaction-data))
  (set-transaction-stack-tag! (thread-cell-ref current-transaction) t))



;; for internal use only

;; modes: read write read/write read:restart

;; transaction-begin: TMode Continuation Continuation (-> Any ...) -> Transaction

(define (transaction-begin mode abort-thunk restart-thunk [restartable-method-thunk #f])
  (unless (thread-cell-ref current-transaction)
    (init-transaction-data))
  (define t (thread-cell-ref current-transaction))
  (when (= (transaction-stack-count t) (transaction-stack-length t))
    (let ([ns (arithmetic-shift (transaction-stack-length t) 1)]
          [old-tables (transaction-stack-write-tables t)]
          [old-queues (transaction-stack-queues t)]
          [old-modes (transaction-stack-modes t)]
          [old-aborts (transaction-stack-abort t)])
      (set-transaction-stack-length! t ns)
      (set-transaction-stack-write-tables! t (build-vector ns #f))
      (vector-copy! (transaction-stack-write-tables t) 0 old-tables)
      (set-transaction-stack-queues! t (build-vector ns #f))
      (vector-copy! (transaction-stack-queues t) 0 old-queues)
      (set-transaction-stack-modes! t (build-vector ns #f))
      (vector-copy! (transaction-stack-modes t) 0 old-modes)
      (set-transaction-stack-abort! t (build-vector ns #f))
      (vector-copy! (transaction-stack-abort t) 0 old-aborts)
      (for ([i (in-range (vector-length old-tables) ns)])
        (vector-set! (transaction-stack-write-tables t) i (make-hasheq))
        (vector-set! (transaction-stack-queues t) i (make-queue)))))
  (define outer-mode (transaction-stack-mode t))
  (when (and (eq? mode 'read:restart) (or (zero? (transaction-stack-count t))
                                          (transaction-stack-in-restartable? t)))
    (set! mode 'read)) ; top level restartables become regular reads, nested restartables also become regular reads
  (vector-set! (transaction-stack-modes t) (transaction-stack-count t) mode)
  (vector-set! (transaction-stack-abort t) (transaction-stack-count t) abort-thunk)
  (start-atomic)
  (cond [(zero? (transaction-stack-count t)) ; top level transaction
         ;; at the top level a restartable transaction is the same as a regular read-only transaction, no special case
         (set-transaction-stack-state! t 'transaction)
         (set-transaction-stack-id! t completed-write-transactions)
         (set-transaction-stack-restart! t restart-thunk)
         (when (eq? mode 'read/write) (set-clear! (transaction-stack-read-set t)))
         (unless (or (eq? mode 'read) (eq? mode 'read:restart)) ; read doesn't need write tables, but any write mode does
           (hash-clear! (vector-ref (transaction-stack-write-tables t) 0)))
         (queue-clear! (transaction-stack-restartable-queue t))
         (add-transaction t)
         (end-atomic)
         ; (struct restartable-method-record (read-set [return #:mutable] proc))
         ]
        [(and restartable-method-thunk (not (transaction-stack-in-restartable? t)))
         (end-atomic)
         (queue-add-back! (transaction-stack-restartable-queue t) (restartable-method-record (mutable-seteq) #f restartable-method-thunk))
         (set-transaction-stack-in-restartable?! t #t)
         ]
        [(compatible-modes? outer-mode mode)
         (end-atomic)
         (unless (eq? outer-mode 'read) ; read doesn't need write tables, but any write mode does
           (hash-clear! (vector-ref (transaction-stack-write-tables t) (transaction-stack-count t))))]
        [(and (or (eq? mode 'read/write)
                  (eq? mode 'write))
              (transaction-stack-in-restartable? t))
         (end-atomic)
         (raise-transaction-error 'transaction-start "nested write mode not permitted during restartable read method")]
        [else
         (end-atomic)
         (transaction-restart)])
  (set-transaction-stack-count! t (add1 (transaction-stack-count t)))
  t)

;; (transaction-restart) restarts the current top-level transaction in read/write mode.
;;   This is triggered by
;;     - trying to start a nested transaction in a conflicting mode (nothing conflicts with r/w, so problem fixed!)
;;     - reading from a stale vbox when the written-set is non-empty (transaction will fail to commit and restart anyway, don't waste time)
;;     - writing to a vbox when a stale read has occurred (ditto)
;;   This is NOT triggered by a failure to commit.  (See call-with-transaction)

(define (transaction-restart)
  (define t (thread-cell-ref current-transaction))
  ;(eprintf "restart triggered\n")
  ;; restarts happen because: read-only or write-only hint was wrong, or a stale read occurred during read/write
  ;; therefore: mode is always read/write after a mid-transaction restart
  ;; escape continuation with indication of incomplete transaction (do not attempt to commit, roll back and restart in read/write mode)
  ((transaction-stack-restart t) 'incomplete))

;; (make-vbox initial) creates a vbox with value initial
;; make-vbox: X -> (VBoxof X)

(define (make-vbox initial)
  (if (in-transaction?)
      (let ([new-box (vbox (vbody 0 'invalid #f))]) ;; 
        (vbox-set! new-box initial)
        new-box)
      (vbox (vbody completed-write-transactions initial #f))))

;; (vbox-ref vb) returns vb's value.
;; vbox-ref: (VBoxof X) -> X
;;   if in transaction: vb's value is the last value written during current transaction, or else vb's value at the moment transaction was started
;;   if not in transaction: vb's value is the most recent committed value

(define (vbox-ref vb)
  (let* ([t (thread-cell-ref current-transaction)]
         [id (and t (transaction-stack-id t))]
         [state (and t (transaction-stack-state t))])
    (case state
      [(commit)
       (raise-transaction-error 'vbox-ref "illegal vbox access during defered procedure call")]
      [(transaction)
       (when (eq? (transaction-stack-mode t) 'write)
         (transaction-restart))
       (define body
         ;; Walk backward in transactions looking for a write to vb
         ;; if none found, use vb's global history
         (let loop ([i (sub1 (transaction-stack-count t))])
           (cond [(negative? i)
                  (set-add! (transaction-reads t) vb)
                  (vbox-body vb)]
                 [(hash-has-key? (vector-ref (transaction-stack-write-tables t) i) vb)
                  (hash-ref (vector-ref (transaction-stack-write-tables t) i) vb)]
                 [else (loop (sub1 i))])))
       ;; If using vb's global history, walk back in time to the most recent write from before t began
       (if (vbody? body)
           (let loop ([body body])
             (unless (vbody? body)
               (error 'vbox-ref "vbox-ref during transaction with id ~a (tag ~a) hit vbox with no memory prior to id (~a)\nglobal id is ~a\noldest transaction is ~a" id
                      (transaction-stack-tag t)
                      (vbox-body vb)
                      completed-write-transactions
                      (bin-heap-min transaction-heap)))
             (if (<= (vbody-id body) id)
                 (vbody-value body)
                 (loop (vbody-next body))))
           body)]
      [else ; not in transaction, or read is part of restartable read and therefore atomic
       (vbody-value (vbox-body vb))])))


;; (vbox-set! vb v) sets versioned-box vb to value v within the current transaction
;;   if no current transaction, immediately commits a write-only transaction that writes v to vb
;; vbox-set!: (VBoxof X) X -> Void

(define (vbox-set! vb v)
  (define t (thread-cell-ref current-transaction))
  (cond
    [(and t (or (eq? (transaction-stack-state t) 'commit)
                (eq? (transaction-stack-state t) 'restart)))
     (raise-transaction-error 'vbox-set! "illegal vbox access during defered procedure call")]
    [(and t (eq? (transaction-stack-state t) 'transaction))
     (when (transaction-stack-in-restartable? t)
       (raise-transaction-error 'vbox-set! "write access attempted in read-only method transaction"))
     (when (memq (transaction-stack-mode t) '(read read:restart))
       (transaction-restart))
     (hash-set! (vector-ref (transaction-stack-write-tables t)
                            (sub1 (transaction-stack-count t)))
                vb v)]
    [else
     (start-atomic)
     (set! completed-write-transactions (add1 completed-write-transactions))
     (set-vbox-body! vb (vbody completed-write-transactions v (vbox-body vb)))
     (define r (transaction-record completed-write-transactions
                                   (hasheq vb v)
                                   #f
                                   (and t (transaction-stack-tag t))))
     (add-transaction-record r)
     (end-atomic)]))

;; (vbox-cas! vb proc) updates vb's contents by applying update procedure proc
;;   this change is done atomically even if done outside of a transaction
;;     this is substantially lighter weight than a full transaction so this is the
;;     preferred way to perform any updates that change only a single vbox
;; vbox-cas!: (VBoxof X) (X -> X) -> Void

(define (vbox-cas! vb proc)
  (define t (thread-cell-ref current-transaction))
  (cond [(and t (or (eq? (transaction-stack-state t) 'commit)
                    (eq? (transaction-stack-state t) 'restart)))
         (raise-transaction-error 'vbox-cas! "illegal vbox access during defered procedure call")]
        [(and t (eq? (transaction-stack-state t) 'transaction))
         (when (transaction-stack-in-restartable? t)
           (raise-transaction-error 'vbox-cas! "write access attempted in read-only method transaction"))
         (unless (eq? (transaction-stack-mode t) 'read/write)
           (transaction-restart))
         (vbox-set! vb (proc (vbox-ref vb)))] ; if in transaction, cas is nothing special, just a read and a write
        [else
         (let loop ()
           (define v (vbox-ref vb))  
           (define v-prime (proc v)) 
           (start-atomic)
           (cond [(eq? v (vbox-ref vb))
                  (set! completed-write-transactions (add1 completed-write-transactions))
                  (set-vbox-body! vb (vbody completed-write-transactions v-prime (vbox-body vb)))
                  (define r (transaction-record completed-write-transactions ; transaction of 1 read/write
                                                (hasheq vb v)
                                                #f ; no sense wasting a semaphore, it's already finished
                                                (and t (transaction-stack-tag t))))
                  (add-transaction-record r)
                  (end-atomic)]
                 [else
                  (end-atomic)
                  (loop)]))])) ; commit failed, retry

         
;; (transaction-consistent? t) produces #t if no vboxes read by t have been updated since t was started
;;  #f otherwise

(define (transaction-consistent? t)
  (define id (transaction-stack-id t))
  (andmap (lambda (vb)
            (<= (vbody-id (vbox-body vb)) id))
          (set->list (transaction-stack-read-set t))))

;; (transaction-restartables-consistent t) produces #t if no vboxes read by any restartable submethods of t
;;   have been updated since t was started *OR* if those methods, when restarted, still produce the same value

(define (transaction-restartables-consistent? t)
  (let ([q (transaction-stack-restartable-queue t)]
        [id (transaction-stack-id t)]
        [old-state (transaction-stack-state t)])
    (set-transaction-stack-state! t 'restartable)
    (begin0
      (if (queue-empty? q) #t
          (let loop ()
            (define rr (queue-remove-front! q))
            (and
             (or (andmap (lambda (vb)  (<= (vbody-id (vbox-body vb)) id))
                         (set->list (restartable-method-record-read-set rr)))
                 (equal? (restartable-method-record-return rr)
                         (call-with-values (restartable-method-record-proc rr) list))) 
           (or (queue-empty? q) (loop)))))
    (set-transaction-stack-state! t old-state))))
    

;; Note that this does not abort the transaction since the dynamic-wind in with-transaction will do so
;; (transaction-rollback values ...) aborts the innermost transaction and replace continuation with abort-values ...
;;   DANGER WILL ROBINSON: WATCH YOUR ARITIES

(define (transaction-rollback . abort-values)
  (define t (thread-cell-ref current-transaction))
  (unless (and t (eq? (transaction-stack-state t) 'transaction))
    (raise-not-in-transaction-error 'transaction-rollback "rollback not permitted when not in transaction"))
  (apply (vector-ref (transaction-stack-abort t) (sub1 (transaction-stack-count t))) abort-values))
  

;(define-syntax-parameter transaction-rollback
;  (lambda (stx)
;    (raise-syntax-error #f "use of transaction-rollback not in transaction body" stx)))

(define (transaction-abort)
;  (eprintf "abort!\n~a" (continuation-mark-set->context (current-continuation-marks)))
  (define t (thread-cell-ref current-transaction))
  (unless (and t (positive? (transaction-stack-count t)))
    (raise-not-in-transaction-error 'transaction-abort "abort not permitted when not in transaction"))
  (set-transaction-stack-count! t (sub1 (transaction-stack-count t)))
  (queue-clear! (vector-ref (transaction-stack-queues t) (transaction-stack-count t)))
  (when (eq? (vector-ref (transaction-stack-modes t) (transaction-stack-count t)) 'read:restart)
    (set-transaction-stack-in-restartable?! t #f))
  (when (zero? (transaction-stack-count t))
    (semaphore-post transaction-complete-semaphore))) ; signal that top level transaction is finished


(define (defer . proc+args)
  (define t (thread-cell-ref current-transaction))
  (cond [(and t (positive? (transaction-stack-count t)))
         (queue-add-back! (vector-ref (transaction-stack-queues t) (sub1 (transaction-stack-count t)))
                          proc+args)]
        [else (apply (car proc+args)
                     (cdr proc+args))]))


(define (in-transaction?)
  (define t (thread-cell-ref current-transaction))
  (and t (positive? (transaction-stack-count t))))

(define (transaction-commit result)
  (let ([t (thread-cell-ref current-transaction)])
    (unless (and t (positive? (transaction-stack-count t)))
      (raise-not-in-transaction-error 'transaction-commit "commit not permitted when when not in transaction"))
    (let* ([c (sub1 (transaction-stack-count t))]
          [mode (vector-ref (transaction-stack-modes t) c)])
      (set-transaction-stack-count! t c)

      ;(eprintf "commit transaction in mode ~a\n" mode)
      (cond [(positive? c)
             (when (or (eq? mode 'read/write)
                       (eq? mode 'write))
               (hash-for-each (vector-ref (transaction-stack-write-tables t) c)
                              (lambda (vb v)
                                (hash-set! (vector-ref (transaction-stack-write-tables t) (sub1 c)) vb v))))
             (queue-copy-clear! (vector-ref (transaction-stack-queues t) (sub1 c))
                                (vector-ref (transaction-stack-queues t) c))
             (when (eq? mode 'read:restart)
               (set-restartable-method-record-return! (queue-peek-back (transaction-stack-restartable-queue t)) result)
               (set-transaction-stack-in-restartable?! t #f))
             
             #t]
            ;; TODO read:restart
            [else
             (let ([rs? (or (eq? mode 'write) (set-empty? (transaction-stack-read-set t)))]
                   [wt (vector-ref (transaction-stack-write-tables t) 0)]
                   [wt? (or (eq? mode 'read) (eq? mode 'read:restart) (hash-empty? (vector-ref (transaction-stack-write-tables t) 0)))])
               (start-atomic)
               (set-transaction-stack-state! t 'commit)
               (when (eq? mode 'read:restart)
                 (set-transaction-stack-in-restartable?! t #f))
               (define valid-transaction?
                 (and (or rs?
                          wt?
                          (transaction-consistent? t))
                      (transaction-restartables-consistent? t)))
               (unless valid-transaction? (set! restart-count (add1 restart-count)))
               (when valid-transaction?
                 (let ([new-id (add1 completed-write-transactions)])
                   (when (and (or (eq? mode 'read/write) (eq? mode 'write)) (not wt?))
                     (set! completed-write-transactions new-id)
                     (set-transaction-record-writes! (transaction-stack-record t) (hash-copy wt))
                     (hash-for-each wt
                                    (lambda (vb v)
                                      (set-vbox-body! vb
                                                      (vbody new-id v (vbox-body vb))))))))
               (end-atomic)
               (semaphore-post (transaction-stack-semaphore t))
               (when (and valid-transaction? (not (queue-empty? (vector-ref (transaction-stack-queues t) 0))))
                 (for ([event (in-queue (vector-ref (transaction-stack-queues t) 0))])
                   (apply (car event) (cdr event))))
               (queue-clear! (vector-ref (transaction-stack-queues t) 0))
               (set-transaction-stack-state! t 'none)
               valid-transaction?)]))))
                       

(define-syntax (with-transaction stx)
  (syntax-parse stx 
    [(with-transaction (~optional (~seq #:mode mode:expr)) body ...)
     (with-syntax ([mode/stx (or (attribute mode) #'read/write)])
       (syntax/loc stx
         (call-with-transaction (lambda () body ...) #:mode 'mode/stx)))]))

(define (call-with-transaction thunk #:mode [mode 'read/write])
  (cond [(in-transaction?)
         (define results #f)
         (let/ec abort-ec ;; jumping with abort-ec will abort the transaction, but only the inner one
           (dynamic-wind
             (lambda ()
               (transaction-begin mode abort-ec #f (if (eq? mode 'read:restart) thunk #f)))
             (lambda ()
               (call-with-continuation-barrier
                (lambda ()
                  (set! results (call-with-values thunk list)))))
             (lambda ()
               (unless results
                 (transaction-abort))))
           (when results
             (if (transaction-commit results) (apply values results) (raise-transaction-error 'with-transaction "impossible has occurred, a nested transaction failed to commit"))))]
        [else
         (define results #f)
         (let/ec abort-ec
           (let loop ([mode mode])
             (let/ec restart-ec ;; only top level transactions have a restart continuation (nested transaction don't restart, other than restartable methods, a whole different thingy)
               (dynamic-wind
                 (lambda ()
                   (transaction-begin mode abort-ec restart-ec))
                 (lambda ()
                   (call-with-continuation-barrier
                    (lambda ()
                      (set! results (call-with-values thunk list)))))
                 (lambda ()
                   (unless results
                     (transaction-abort)))))  ; if thunk escaped with a continuation, abort the transaction (continuation barrier will prevent the sneaky SOB from jumping back in)
             (if (and results (transaction-commit results)); if results is #f, dynamic-wind aborted already
                 (apply values results) ; commit successful, return values
                 (loop 'read/write))))])) ; commit failed or early restart called, repeat

#|
(define-syntax (with-transaction stx)
  (syntax-case stx ()
    [(_ body ...)
     #'(let/ec abort-ec
         (let loop ()
             (with-handlers ([(lambda (e) #t)
                              (lambda (e)
                                (transaction-abort)
                                (raise e))])
               (transaction-begin abort-ec)
               (begin0
                   (begin body ...)
                 (unless (transaction-commit)
                   (loop))))))]))
|#
;(provide vbox-history-count)
;(define (vbox-history-count vb)
;  (let loop ([body (vbox-body vb)]
;             [count 1])
;    (if (vbody-next body)
;        (loop (vbody-next body) (add1 count))
;        count)))

;; requires: there exist no active transactions older than id
;; effects: all history older than id is removed from vb, except for one thing

(define (vbox-prune! vb id)
  (let loop ([body (vbox-body vb)])
    (if (and (vbody-next body)
             (> (vbody-id (vbody-next body)) id))
        (loop (vbody-next body))
        (when (vbody-next body)
          (set-vbody-next! (vbody-next body) #f)))))


;; (transaction-cleanup! boxes id) deletes all history from boxes that is older than id
;; transaction-cleanup! (Setof VBox) Nat -> Void
;; requires: no running transaction have id < id (i.e. history that is < id will never be reached by vbox-ref, so it's safe to delete)

(define (transaction-cleanup! boxes id)
  (for ([vb (in-hash-keys boxes)])
    (vbox-prune! vb id))) ; never prunes the head, and never prunes history that's still active, so this is thread safe!


(define (transaction<=? t1 t2)
  (<= (transaction-record-id t1)
      (transaction-record-id t2)))

(define transaction-heap (make-bin-heap transaction<=?))
(define transaction-semaphore (make-semaphore 0)) ; if there's a fresh transaction on top of the heap
(define transaction-complete-semaphore (make-semaphore 0)) ; to signal a finished transaction

(define (add-transaction t)
  (define result (transaction-record (transaction-stack-id t)
                                     #f
                                     (transaction-stack-semaphore t)
                                     (transaction-stack-tag t)))
  (set-transaction-stack-record! t result)    
  (add-transaction-record result))

(provide print-transaction-debug-info)
(define (print-transaction-debug-info)
  (start-atomic)
  (let ([c (bin-heap-count transaction-heap)]
        [r (if (zero? (bin-heap-count transaction-heap)) 'n/a (bin-heap-min transaction-heap))])
    (end-atomic)
    (eprintf "number of active transactions: ~a\n" c)
    (eprintf "record for oldest transaction: ~a\n" r)))

(define (add-transaction-record r)
  (let ([atomic? (in-atomic-mode?)])
    (unless atomic? (start-atomic))
    (define c (bin-heap-count transaction-heap))
    (bin-heap-add! transaction-heap r)
    (unless atomic? (end-atomic))
    (when (zero? c) ; because IDs are non-decreasing, new root only if heap was empty
      (semaphore-post transaction-semaphore))))

(define (transaction-thunk)
  ; pre: heap is empty
  (semaphore-wait transaction-semaphore) ; wait until not empty
  (define written-set (make-hasheq))
  (let loop ()  
    (start-atomic) ; heap not thead safe, all access kept atomic
    (if (positive? (bin-heap-count transaction-heap))
        (let ([t (bin-heap-min transaction-heap)])
          (end-atomic)
          (when (transaction-record-semaphore t)
            (unless (semaphore-try-wait? (transaction-record-semaphore t)) ; if the next transaction isn't finishe
              (transaction-cleanup! written-set (transaction-record-id t)) ; may as well pass the time by cleaning up unused history
              (hash-clear! written-set)
              (semaphore-wait (transaction-record-semaphore t))))
          ;(and (transaction-record-semaphore t) (semaphore-wait (transaction-record-semaphore t)))
          (start-atomic)
          (bin-heap-remove-min! transaction-heap)
          (end-atomic)
          (when (transaction-record-writes t)
            (for ([vb (in-hash-keys (transaction-record-writes t))])
              (hash-set! written-set vb (transaction-record-id t))))
          (loop))
        (end-atomic)))
  (start-atomic)
  (let ([id (if (zero? (bin-heap-count transaction-heap)) 
                completed-write-transactions ; no new transactions since loop ended, erase all history
                (transaction-record-id (bin-heap-min transaction-heap)))]) ; new transactions since loop ended, erase only history older than
    ; oldest active transaction
    (end-atomic)
    (transaction-cleanup! written-set id)
    (hash-clear! written-set))
  (transaction-thunk))
      

(define history-eraser-thread
  (thread transaction-thunk))



(module+ test
  (require rackunit)


  (define counter (make-vbox 0))
  (define t1sema (make-semaphore 0))
  (define thread1
    (thread (lambda ()
              (thread-receive)
              (with-transaction
                (vbox-set! counter (add1 (vbox-ref counter)))))))

  
  (define t2sema (make-semaphore 0))
  (define thread2
    (thread (lambda ()
              (with-transaction
                (semaphore-post t2sema)
                (vbox-set! counter (add1 (vbox-ref counter)))
                (thread-receive)
                (defer (lambda (v)
                         (unless (= 2 v)
                           (error 'deferred-action "finalizer called even through transaction was rolled back!")))
                  (vbox-ref counter))))))

  (semaphore-wait t2sema)
  (thread-send thread1 'go) ; thread 1 will complete and change counter
  (thread-wait thread1)

  (check-eqv? (vbox-ref counter) 1)
  (check-eq? (thread-running? thread1) #f)

  (thread-send thread2 'go) ; thread 2 should fail and restart
  (semaphore-wait t2sema)

  (check-eqv? (vbox-ref counter) 1)
  
  
  (thread-send thread2 'go-again) ; thread 2 should succeed this time
  (thread-wait thread2)

  (check-eqv? (vbox-ref counter) 2)
  (check-eq? (thread-running? thread2) #f)

  ;; restarts
  
  (define my-box (make-vbox 0))
  (define my-box-counter 0)
  (with-transaction #:mode read
    (set! my-box-counter (add1 my-box-counter)) ;; Don't have side effects in a transaction. Nyah!
    (vbox-set! my-box (add1 (vbox-ref my-box))))
  (check-eqv? (vbox-ref my-box) 1)
  (check-eqv? my-box-counter 2) ; the read mode transaction should have restarted in read/write mode

  (with-transaction #:mode write
    (set! my-box-counter (add1 my-box-counter))
    (vbox-set! my-box (add1 (vbox-ref my-box))))
  (check-eqv? (vbox-ref my-box) 2)
  (check-eqv? my-box-counter 4) ; the write mode transaction should have restarted in read/write mode

  (with-transaction #:mode write
    (vbox-set! my-box 42))
  (check-eqv? (vbox-ref my-box) 42)


  (define restartable-counter 0)
  (define outer-counter 0)
  (set! thread1
        (thread (lambda ()
                  (with-transaction
                    (set! outer-counter (add1 outer-counter))
                    (semaphore-post t1sema)
                    (thread-receive)
                    (define x (with-transaction #:mode read:restart
                                (set! restartable-counter (add1 restartable-counter))
                                (vbox-ref counter)
                                (vbox-ref my-box)))
                    (vbox-set! my-box (add1 x))))))


  (set! thread2
        (thread (lambda ()
                  (thread-receive)
                  (with-transaction #:mode write
                    (vbox-set! counter 0)))))


  (semaphore-wait t1sema)
  ;; at this point - thread1 is stalled during a transaction that r/w my-box, with nested restartable that will read counter + mybox
  (thread-send thread2 'go) ; thread2 will reset counter to 0, causing the nested restartable to become inconsistent
  (thread-wait thread2) ; write transaction now comitted

  (thread-send thread1 'go) ; thread1 will continue with transaction.  nested should fail and restart, allowing outer to commit first time
  (thread-wait thread1)
  
  (check-eqv? outer-counter 1) ; outer transaction did not restart
  (check-eqv? restartable-counter 2) ; inner transaction did restart to confirm equal return value
  (check-eqv? (vbox-ref my-box) 43) ; outer transaction actually comitted
  (check-eqv? (vbox-ref counter) 0) ; write transaction actually comitted
  
  
)
