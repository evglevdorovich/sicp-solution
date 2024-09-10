#lang sicp

(define (equal-proc? a b)
    (cond
        ((or (null? a) (null? b)) (and (null? a) (null? b)))
        ((and (pair? (car a)) (pair? (car b)))
                    (and (equal-proc? (car a) (car b)) (equal-proc? (cdr a) (cdr b)))
        )
        (else (and (eq? (car a) (car b)) (equal-proc? (cdr a) (cdr b))))
    )
)

 (equal-proc? '(1 2 3 (4 5) 6) '(1 2 3 (4 5 7) 6))
