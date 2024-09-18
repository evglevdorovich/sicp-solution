#lang sicp

(define (element-of-set? x set)
(cond ((null? set) false)
((equal? x (car set)) true)
(else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
(if (element-of-set? x set)
set
(cons x set)))

(define (union-set set1 set2)
    (if (null? set2) set1
        (union-set (adjoin-set (car set2) set1) (cdr set2))
    )
)

(define first '(1 0 5 2 9))
(define second '(10 1 19 5 50))
(define result (union-set first second))

(union-set first '())

(union-set '() second)
