#lang sicp
(define (square a ) (* a a))

(define (iter things answer)
    (if (null? things)
    answer
        (iter (cdr things)
        (cons answer
        (square (car things))))
        )
    )
(iter items nil)

(square-list2 (list 1 2 3 4 5))
