#lang racket/base

(require versioned-box racket/unsafe/ops)

(struct vhashtable (table key-list-head key-list-tail count mode))
(struct vhashset vhashtable ())
(struct vhashmap vhashtable ())

(struct vhashnode (key next key-next key-prev))
(struct vhashmapnode vhashnode (value))


(define initial-size 128)
(define alpha-factor 2)


(define (make-vhashset mode)
  (unless (memq mode '(eq eqv equal))
    (raise-argument-error 'make-hashset "(or/c 'eq 'eqv 'equal)" mode))
  (vhashset (make-vbox (build-vector initial-size (lambda (i) (make-vbox null))))
            (make-vbox null) (make-vbox null)
            (make-vbox 0)
            mode))


(define (make-vhashmap mode)
  (unless (memq mode '(eq eqv equal))
    (raise-argument-error 'make-hashset "(or/c 'eq 'eqv 'equal)" mode))
  (vhashmap (make-vbox (build-vector initial-size (lambda (i) (make-vbox null))))
            (make-vbox null) (make-vbox null)
            (make-vbox 0)
            mode))


(define (vhashtable-find* ht key)
  (let ([i ((case (vhashtable-mode ht)
              [(eq) eq-hash-code]
              [(eqv) eqv-hash-code]
              [(equal) equal-hash-code]) key)]
        [match? (case (vhashtable-mode ht)
                  [(eq) eq?]
                  [(eqv) eqv?]
                  [(equal) equal?])]
        [table (vbox-ref (vhashtable-table ht))])
  (let ([link (vector-ref table (unsafe-fxmodulo i (vector-length table)))])
    (let loop ([node (vbox-ref link)]
               [prev link])
      (cond [(or (null? node) (match? key (vhashnode-key node))) (values node prev)]
            [else (let ([next (vhashnode-next node)])
                    (loop (vbox-ref next) next))])))))


(define (vhashmap-set! hm key value)
    (with-transaction
      (let-values ([(node next) (vhashtable-find* hm key)])
        (cond [(null? node) ;                    
               (let* ([tail-node (vbox-ref (vhashtable-key-list-tail hm))]
                      [new-node  (vhashmapnode key                   
                                               (make-vbox null)      ; next node in chain
                                               (make-vbox null)      ; next node in key-order LL
                                               (make-vbox tail-node) ; prev node in key-order LL
                                               (make-vbox value))])  
                 (vbox-set! (if (null? tail-node)
                                (vhashtable-key-list-head hm) ; no prev key, so this is now the head and tail
                                (vhashnode-key-next tail-node))
                            new-node)
                 (vbox-set! (vhashtable-key-list-tail hm) new-node) ; this is the last inserted key
                 (vbox-set! next new-node))] ; don't forget to actually put it into the hash bin's chain
              [else
               (vbox-set! (vhashmapnode-value node) value)])))) ; boring, key already exists, just one update


(define (vhashmap-remove! hm key)
  (vhashtable-remove! hm key))

(define (vhashset-remove! hm key)
  (vhashtable-remove! hm key))

(define (vhashtable-remove! ht key)
  (with-transaction
    (let-values ([(node next) (vhashtable-find* ht key)])
      (unless (null? node)
        (vbox-set! next (vbox-ref (vhashnode-next node))) ; spliced out of chain
        (let ([key-prev (vbox-ref (vhashnode-key-prev node))]
              [key-next (vbox-ref (vhashnode-key-next node))])
          (vbox-set! (if (null? key-prev) (vhashtable-key-list-head ht) (vhashnode-key-next key-prev)) key-next)
          (vbox-set! (if (null? key-next) (vhashtable-key-list-tail ht) (vhashnode-key-prev key-next)) key-prev))))))
        


(define (vhashtable-keys ht)
  (with-transaction #:mode read
    (let loop ([node (vhashtable-key-list-tail ht)]
               [so-far null])
      (if (null? node) so-far
          (loop (vbox-ref (vhashnode-key-prev node))
                (cons (vhashnode-key node) so-far))))))

(define (vhashmap-values hm)
  (with-transaction #:mode read
    (let loop ([node (vhashtable-key-list-tail hm)]
               [so-far null])
      (if (null? node) so-far
          (loop (vbox-ref (vhashnode-key-prev node))
                (cons (vhashmapnode-value node) so-far))))))
  

(define (vhashmap-items hm)
  (with-transaction #:mode read
    (let loop ([node (vhashtable-key-list-tail hm)]
               [so-far null])
      (if (null? node) so-far
          (loop (vbox-ref (vhashnode-key-prev node))
                (cons (cons (vhashnode-key node)
                            (vhashmapnode-value node))
                      so-far))))))



(define (vhashmap-ref hm key [failure-result (lambda () (raise-argument-error 'vhashmap-ref "vhashmap-hash-key?" key))])
  (with-transaction #:mode read:restart
    (let-values ([(node next) (vhashtable-find* hm key)])
      (cond [(and (null? node) (procedure? failure-result)) (failure-result)]
            [(null? node) failure-result]
            [else (vbox-ref (vhashmapnode-value node))]))))


(define (vhashset-contains? hs key)
  (vhashtable-has-key? hs key))

(define (vhashmap-has-key? hm key)                                
  (vhashtable-has-key? hm key))
(define (vhashtable-has-key? ht key)
  (with-transaction #:mode read:restart
    (let-values ([(node next) (vhashtable-find* ht key)])
      (not (null? node)))))
