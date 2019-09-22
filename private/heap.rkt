#lang racket/base

(provide bin-heap? make-bin-heap bin-heap-add! bin-heap-count bin-heap-empty? bin-heap-min bin-heap-remove-min!)

(struct bin-heap (key<= [count #:mutable] [length #:mutable] [data #:mutable]))

(define (make-bin-heap key<= [init-size 64])
  (bin-heap key<= 0 init-size (make-vector init-size)))

(define (bin-heap-empty? h)
  (zero? (bin-heap-count h)))

(define (bin-heap-add! h v)
  (when (= (bin-heap-count h)
           (bin-heap-length h))
    (let ([new-length (arithmetic-shift (bin-heap-length h) 1)]
          [old-vector (bin-heap-data h)])
      (set-bin-heap-length! h new-length)
      (set-bin-heap-data! h (make-vector new-length))
      (vector-copy! (bin-heap-data h) 0 old-vector)))
  (define key<= (bin-heap-key<= h))
  (define data (bin-heap-data h))
  
  (let loop ([i (bin-heap-count h)])
    (if (positive? i)
      (let* ([parent-i (arithmetic-shift (sub1 i) -1)]
             [parent (vector-ref data parent-i)])
        (if (key<= parent v)
            (vector-set! data i v)
            (begin
              (vector-set! data i parent)
              (loop parent-i))))
        (vector-set! data i v)))

  (set-bin-heap-count! h (add1 (bin-heap-count h))))

(define (bin-heap-min h)
  (when (zero? (bin-heap-count h))
    (error 'bin-heap-min "min value of empty heap is not defined"))
  (vector-ref (bin-heap-data h) 0))

(define (bin-heap-remove-min! h)
  (when (zero? (bin-heap-count h))
    (error 'bin-heap-remove-min! "min value of empty heap is not defined"))
  (define key<= (bin-heap-key<= h))
  (define count (sub1 (bin-heap-count h)))
  (define data (bin-heap-data h))
  (set-bin-heap-count! h count)
  (unless (zero? count)
    (let ([v (vector-ref data count)])
      (let loop ([i 0])
        (define 2i (arithmetic-shift i 1)) ; 10% faster than (* 2 i), and 33% faster than doing it each time instead of caching
        (if (>= (add1 (* 2 i)) count) ; i has no children, it's where v belongs by default
            (vector-set! data i v)
            (let* ([c1 (vector-ref data (add1 2i))]
                   [c2 (if (= 2i count) #f (vector-ref data (+ 2 2i)))]
                   [smallest-child (if (or (= 2i count) (key<= c1 c2))
                                       c1
                                       c2)]
                   [smallest-index (if (eq? smallest-child c1) (add1 2i) (+ 2 2i))]) ; if there are duplicates, meh, don't matter which we pick
              (if (key<= smallest-child v)
                  (begin
                    (vector-set! data i smallest-child)
                    (loop smallest-index))
                  (vector-set! data i v))))))))
  

  
  
#|(define my-heap (make-heap <=))
(define my-heap2 (make-bin-heap <=))
(define n 1000000)
(define n-list (build-list n (lambda (i) (random n))))

(for ([v (in-list n-list)])
  (bin-heap-add! my-heap2 v))

(time
 (for ([i (in-range n)])
   (bin-heap-remove-min! my-heap2)))

|#


