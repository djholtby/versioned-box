#lang racket/base

(require versioned-box racket/unsafe/ops)

(struct vhashtable (table key-list count mode))
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
            (make-vbox null)
            (make-vbox 0)
            mode))


(define (make-vhashmap mode)
  (unless (memq mode '(eq eqv equal))
    (raise-argument-error 'make-hashset "(or/c 'eq 'eqv 'equal)" mode))
  (vhashmap (make-vbox (build-vector initial-size (lambda (i) (make-vbox null))))
            (make-vbox null)
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
  (let ([link (vector-ref table (unsafe-fxmoduli i (vector-length table)))])
    (let loop ([node (vbox-ref link)]
               [prev link])
      (cond [(null? node) (values node prev)]
            [(match? key (if (cons? node) (car node) (vhashnode-key node)))
             (values node prev)]
            [else (let ([next (if (cons? node) (cdr node) (vhashnode-next node))])
                    (loop (vbox-ref next) next))])))))


(define (vhashmap-set! hm key value)
    (with-transaction
      (let-values ([(node next) (vhashtable-find* hm key)])
        (cond [(null? node)
               (let ([kn (vmapkeynode key (make-vbox null) (make-vbox (vbox-ref (vhashmap-keyslist hm))))])
                 (vbox-set! (vhashmap-keyslist hm) kn)
                 (vbox-set! prev (vmapnode key (make-vbox value) (make-vbox null) kn)))]
              [else
               (vbox-set! (vmapnode-value node) value)])))))


(define (vhashmap-remove! hm key)
  (with-transaction
    (let-values ([(node next) (vhashtable-find* hm key)])
      (unless (null? node)
        (vbox-set! next (vbox-ref (vmapnode-next node))) ; spliced out of chain
        ;; next, splice out of keys linked list
        (let ([kchain-prev (vbox-deref (vmapkeynode-prev (vmapnode-keyref node)))] ; 
              [kchain-next (vbox-deref (vmapkeynode-next (vmapnode-keyref node)))])
          (vbox-set! (if (null? kchain-prev) (vhashtable-keyslist hm) (vmapkeynode-next kchain-prev)) kchain-next)
          (unless (null? kchain-next) ; 
            (vbox-set! (vmapkeynode-prev kchain-next) kchain-prev)))))))


(define (vhashmap-keys hm)
  (with-transaction #:mode read
    (let loop ([node (vhashmap-keyslist hm)]
               [so-far null])
      (if (null? node) so-far
          (loop (vbox-ref (vmapkeynode-next node))
                (cons (vmapkeynode-key node) so-far))))))


(define (vhashmap-ref hm key [failure-result (lambda () (raise-argument-error 'vhashmap-ref "vhashmap-hash-key?" key))])
  (with-transaction #:mode read:restart
    (let-values ([(node next) (vhashtable-find* hm key)])
      (cond [(and (null? node) (procedure? failure-result)) (failure-result)]
            [(null? node) failure-result]
            [else (vbox-ref (vmapnode-value node))]))))

        
(define (vhashmap-has-key? hm key)                                
  (with-transaction #:mode read:restart
    (let-values ([(node next) (vhashtable-find* hm key)])
      (not (null? node)))))
