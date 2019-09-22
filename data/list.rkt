#lang racket/base

(require versioned-box)

(struct vlist (head))

(define (make-vlist)
  (vlist (make-vbox null)))

(define (vlist-add-front! vlst v)
  (vbox-cas! (vlist-head vlst)
             (lambda (next)
               (cons (vbox v)
                     (vbox next)))))

(define (vlist-remove-front! vlst)
  (with-transaction
    (let ([v (vbox-ref (car (vbox-ref (vlist-head vlst))))])
      (vbox-cas! (vlist-head vlst) car)
      v)))

(define (vlist->list vlst)
  (reverse
   (with-transaction #:mode read
     (let loop ([node (vbox-ref (vlist-head vlst))]
                [so-far null])
       (if (null? node) so-far
           (loop (vbox-ref (cdr node))
                 (cons (vbox-ref (car node)) so-far)))))))


(define (vlist-member? vlst v)
  (vlist-memf?* vlst v (curry equal? v)))

(define (vlist-memv? vlst v)
  (vlist-memf? vlist v (curry eqv? v)))

(define (vlist-memq? vlst v)
  (vlist-memf? vlst (curry eq? v)))

(define (vlist-memf? vlst match?)
  (with-transaction #:mode read:restart
    (let loop ([node (vbox-ref (vlist-head vlst))])
      (cond [(null? node) #f]
            [(equal? v (vbox-ref (car node))) #t]
            [else (loop (vbox-ref (cdr node)))]))))

(define (vlist-filter! vlst keep?)
  (with-transaction
    (let ([last-link
           (let loop ([node (vbox-ref (vlist-head vlst))]
                      [prev (vlist-head vlst)])
             (cond [(null? node) prev]
                   [(keep? (vbox-ref (car node)))
                    (unless (eq? node (vbox-ref prev))
                      (vbox-set! prev node)
                      (loop
                       (vbox-ref (cdr node))
                       (cdr node)))]
                   [(loop (vbox-ref (cdr node)) prev)]))])
      (unless (null? (vbox-ref last-link))
        (vbox-set! last-link null)))))

(define (vlist-filter vlst keep?)
  (filter (vlist->list vlst) keep?))
             
