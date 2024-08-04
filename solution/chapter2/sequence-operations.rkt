#lang sicp

(define (odd? n)
        (= (remainder n 2) 1)
)

(define (filter predicate sequence)
    (if (null? sequence) nil
    (if (predicate (car sequence))
    (cons (car sequence) (filter predicate (cdr sequence)))
    (filter predicate (cdr sequence))
    ))
)

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
        (accumulate op initial (cdr sequence))
        )
    )
)

(define (enumerate-interval low high)
    (if (> low high) nil
        (cons low (enumerate-interval (+ 1 low) high))
    )
)

(define (enumerate-tree tree)
    (cond ((null? tree) '())
          ((not (pair? tree)) (list tree))
          (else (append (enumerate-tree (car tree)) (enumerate-tree (cdr tree))))
    )
)
; (1 (2 (3 4)) 5)
(enumerate-tree (list 1 (list 2 (list 3 4)) 5))
; (1 2 3 4 5)

(filter odd? (list 1 2 3 4 5 6 7 8))
(accumulate + 0 (list 1 2 3 4 5))
(accumulate * 1 (list 1 2 3 4 5))
(accumulate cons nil (list 1 2 3 4 5))
(enumerate-interval 2 7)
;(2 3 4 5 6 7)
