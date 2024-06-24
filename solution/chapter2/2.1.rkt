#lang sicp

(define (gcd a b)
    (if (= b 0)
    a
    (gcd b (remainder a b)))
)

(define (make-rat numer denom)

        (if (< denom 0)
            (if (> numer 0) (make-rat (- numer) (abs denom))
                (make-rat (abs numer) (abs denom))
            )
            (let ((g (gcd (abs numer) denom)))
            (cons (/ numer g) (/ denom g))
        )
    )
)

(define (numer rat)
    (car rat)
)

(define (denom rat)
    (cdr rat)
)

(define b (make-rat 3 4))

(define (print-rat rat)
    (newline)
    (display (numer rat))
    (display "/")
    (display (denom rat))
)

(print-rat (make-rat (- 1) 3))
