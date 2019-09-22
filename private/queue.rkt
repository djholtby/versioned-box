#lang racket/base

(provide queue? queue-count make-queue queue-add-back! queue-remove-front!
         queue-empty? queue-clear! queue-copy-clear! queue-copy in-queue queue-peek-front queue-peek-back)

(struct queue (front back count) #:mutable)

(define (make-queue)
  (queue null null 0))

(define (queue-peek-front q)
  (mcar (queue-front q)))

(define (queue-peek-back q)
  (mcar (queue-back q)))

(define (queue-add-back! q e)
  (define new-node (mcons e null))
  (cond [(null? (queue-back q))
         (set-queue-front! q new-node)]
        [else
         (set-mcdr! (queue-back q) new-node)])
  (set-queue-back! q new-node)
  (set-queue-count! q (add1 (queue-count q))))

(define (queue-empty? q)
  (null? (queue-front q)))

(define (queue-remove-front! q)
  (define result (mcar (queue-front q)))
  (set-queue-front! q (mcdr (queue-front q)))
  (when (null? (queue-front q))
    (set-queue-back! q null))
  (set-queue-count! q (sub1 (queue-count q)))
  result)

(define (queue-clear! q)
  (set-queue-count! q 0)
  (set-queue-front! q null)
  (set-queue-back! q null))

(define (queue-copy-clear! dst src)
  (cond [(null? (queue-back dst))
         (set-queue-front! dst (queue-front src))
         (set-queue-back! dst (queue-back src))]
        [else
         (set-mcdr! (queue-back dst)
                    (queue-front src))
         (set-queue-back! dst (queue-back src))])
  (set-queue-count! dst (+ (queue-count src) (queue-count dst)))
  (queue-clear! src))
               
(define (in-queue q)
  (if (null? (queue-front q)) (in-list (queue-front q)) (in-mlist (queue-front q))))

(define (queue-copy q)
  (define result (make-queue))
  (for ([v (in-queue q)])
    (queue-add-back! result v))
  result)

(module+ test
  (define q1 (make-queue))
  (define q2 (make-queue))

  (for ([i (in-range 20)])
    (queue-add-back! q1 i))

  (for ([i (in-range 20 25)])
    (queue-add-back! q2 i))
        
  (displayln "testing in-queue")
  (for ([v (in-queue q1)])
    (displayln v))

  (for ([v (in-queue q2)])
    (displayln v))

  (queue-copy-clear! q2 q1)

  (displayln (format "q1 should be empty:  (queue-empty? q1) => ~a" (queue-empty? q1)))

  (displayln "testing copy-clear!")

  (for ([v (in-queue q2)])
    (displayln v))

  (displayln "testing remove-front")
  
  (for ([i (in-range 25)])
    (displayln (queue-remove-front! q2)))

  (displayln (queue-empty? q2))

  (queue-add-back! q1 'q1)
  (queue-add-back! q2 'q2)
  
  (displayln (queue-remove-front! q1))
  (displayln (queue-remove-front! q2))

  (displayln (queue-empty? q1))
  (displayln (queue-empty? q2)))
  
