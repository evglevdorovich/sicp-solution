#lang sicp

(define (last-pair items)
    (if (null? (cdr items)) items
        (last-pair (cdr items))
    )
)

(define (reverse items)
    (if (null? (cdr items)) items
        (append (reverse (cdr items)) (list (car items)))
    )
)

(define list1 (list 1 2 3 4))
(define list2 (list 5 6 7 8))

(list-ref (list 1 2 3 4) 2)

(last-pair list2)
(reverse list2)
