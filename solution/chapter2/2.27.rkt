#lang sicp

(define (reverse items)
    (if (null? (cdr items)) items
        (append (reverse (cdr items)) (list (car items)))
    )
)

(define (deep-reverse items)
    (cond
        ((not (pair? items)) items)
        (else
            (append (deep-reverse (cdr items)) (list (deep-reverse (car items))))
        )
    )
)


