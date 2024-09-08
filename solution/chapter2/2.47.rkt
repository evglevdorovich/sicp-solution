#lang sicp

(define (make-vect x y)
    (cons x y)
)

(define (make-frame-list origin edge1 edge2)
    (list origin edge1 edge2)
)

(define (make-frame-cons origin edge1 edge2)
    (cons origin (cons edge1 edge2))
)

(define (origin-list frame)
    (car frame)
)

(define (origin-cons frame)
    (car frame)
)

(define (edge1-list frame)
    (cadr frame)
)

(define (edge1-cons frame)
    (cadr frame)
)

(define (edge2-list frame)
    (caddr frame)
)

(define (edge2-cons frame)
    (cddr frame)
)
