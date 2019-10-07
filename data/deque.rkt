#lang racket/base

(require versioned-box)

(struct deque (front back counter))
(struct dq-node (item next prev))

(define (make-deque)
  (deque (make-vbox null) (make-vbox null) (cons (make-vbox 0)
                                                 (tbox 0))))

(define (deque-count! dq amnt)
  (tbox-set! (cdr (deque-counter dq)) (+ amnt (tbox-ref (cdr (deque-counter dq)))))
  (finalizer (lambda () (deque-finalize! dq))))

(define (deque-count dq)
  (with-transaction #:mode read
    (+ (vbox-ref (car (deque-counter dq)))
       (tbox-ref (cdr (deque-counter dq))))))

(define (deque-finalize! dq)
  (let ([c1 (vbox-ref (car (deque-counter dq)))]
        [c2 (tbox-ref (cdr (deque-counter dq)))])
  (vbox-set! (car (deque-counter dq)) (+ c1 c2))
  (tbox-set! (cdr (deque-counter dq)) 0)))

(define (deque-push-front! dq v)
  (with-transaction
    (let* ([old-front (vbox-ref (deque-front dq))]
           [new-node (dq-node v (make-vbox old-front) (make-vbox null))])
      (vbox-set! (deque-front dq) new-node)
      (vbox-set! (if (null? old-front) (deque-back dq) (dq-node-prev old-front)) new-node)
      (deque-count! dq 1))))
  
(define (deque-push-back! dq v)
  (with-transaction
    (let* ([old-back (vbox-ref (deque-back dq))]
           [new-node (dq-node v (make-vbox null) (make-vbox old-back))])
      (vbox-set! (deque-back dq) new-node)
      (vbox-set! (if (null? old-back) (deque-front dq) (dq-node-next old-back)) new-node)
      (deque-count! dq 1))))

(define (deque-peek-front dq)
  (with-transaction #:mode read
    (let ([node (vbox-ref (deque-front dq))])
      (when (null? node)
        (error 'deque-peek-front "peek at empty deque"))
      (dq-node-item node))))

(define (deque-peek-back dq)
  (with-transaction #:mode read
    (let ([node (vbox-ref (deque-back dq))])
      (when (null? node)
        (error 'deque-peek-back "peek at empty deque"))
      (dq-node-item node))))

(define (deque-pop-front! dq [fail-result (lambda () (error 'deque-pop-front! "pop from empty deque"))])
  (with-transaction
    (let ([node (vbox-ref (deque-front dq))])
      (cond [(and (null? node) (procedure? fail-result)) (fail-result)]
            [(null? node) fail-result]
            [else
             (let ([next (vbox-ref (dq-node-next node))])
               (vbox-set! (deque-front dq) next)
               (vbox-set!
                (if (null? next) (deque-back dq) (dq-node-prev next))
                null)
               (deque-count! dq -1)
               (dq-node-item node))]))))

(define (deque-pop-back! dq [fail-result (lambda () (error 'deque-pop-front! "pop from empty deque"))])
  (with-transaction
    (let ([node (vbox-ref (deque-back dq))])
      (cond [(and (null? node) (procedure? fail-result)) (fail-result)]
            [(null? node) fail-result]
            [else
             (let ([prev (vbox-ref (dq-node-prev node))])
               (vbox-set! (deque-back dq) prev)
               (vbox-set! (if (null? prev) (deque-front dq) (dq-node-next prev)) null)
               (deque-count! dq -1)
               (dq-node-item node))]))))

(define (deque-empty? dq)
  (with-transaction #:mode read
    (and (dq-node? (vbox-ref (deque-front dq))) #t)))


(module+ test
  (require rackunit)
  (define my-deque (make-deque))

  (define pusher (thread
                  (lambda ()
                    (for ([i (in-range 10)])
                      (with-transaction
                          (for ([j (in-range 10)])
                            (deque-push-back! my-deque (cons i j))))))))
  (let loop ([remaining 100])
    (unless (zero? remaining)
      (let ([v (deque-pop-front! my-deque #f)])
        (printf "popped ~a, count=~a\n" v (deque-count my-deque))
        (cond [v (loop (sub1 remaining))]
              [else (loop remaining)]))))
  (displayln (format "final restart count: ~a" restart-count))
  )

