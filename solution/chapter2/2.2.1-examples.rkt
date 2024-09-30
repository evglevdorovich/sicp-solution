#lang sicp

(define (list-ref items n)
    (if (= n 0) (car items)
        (list-ref (cdr items) (- n 1))
    )
)

(define (length items)
    (if (null? items) 0
        (+ 1 (length (cdr items)))
    )
)

(define (append l1 l2)
    (if (null? l1)
        l2
         (cons (car l1) (append (cdr l1) l2))
    )
)

(define list1 (list 1 2 3 4))
(define list2 (list 5 6 7 8))

(list-ref (list 1 2 3 4) 2)
(length (list 1 2 3 4))
(append list1 list2)
