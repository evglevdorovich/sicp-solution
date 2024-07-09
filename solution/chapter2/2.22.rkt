#lang sicp

(define (for-each f items)
    (if (null? items) nil)
    (f (car items))
    (if (not (null? (cdr items)))
    (for-each f (cdr items))
    )
)

(for-each (lambda (x)
    (newline)
    (display x))
    (list 57 321 88)
)
