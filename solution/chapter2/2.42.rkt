#lang sicp

(define (square n)
  (* n n))

(define (enumerate-interval low high)
    (if (> low high) nil
        (cons low (enumerate-interval (+ 1 low) high))
    )
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
        (accumulate op initial (cdr sequence))0
        )
    )
)

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

;there

(define (adjoin-position new-row k rest-of-queens)
    (append rest-of-queens
        (map (lambda (row)
            (cons k (list row))
             )
            new-row
        )
    )
)
