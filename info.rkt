#lang info

(define collection "versioned-box")
(define pkg-name "versioned-box")
(define pkg-desc "MVCC using versioned boxes")
(define pkg-authors '(djholtby))
(define scribblings '(("scribblings/versioned-box.scrbl")))
(define deps '("base"))
(define build-deps '("racket-doc"
                     "rackunit-lib"
                     "scribble-lib"))
