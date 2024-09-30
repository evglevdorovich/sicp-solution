#lang sicp

(define (gcd a b)
    (if (= b 0)
    a
    (gcd b (remainder a b)))
)

(define (make-rat numer denom)
    (let ((g (gcd numer denom)))
        (cons (/ numer g) (/ denom g))
    )
)

(define (numer rat)
    (car rat)
)

(define (denom rat)
    (cdr rat)
)

(define b (make-rat 3 4))

(define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
    (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
    (* (numer y) (denom x)))
    (* (denom x) (denom y))))

(define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
    (* (denom x) (denom y))))

(define (div-rat x y)
    (make-rat (* (numer x) (denom y))
    (* (denom x) (numer y))))

(define (equal-rat? x y)
    (= (* (numer x) (denom y))
    (* (numer y) (denom x))))

(define (print-rat rat)
    (newline)
    (display (numer rat))
    (display "/")
    (display (denom rat))
)

(print-rat (add-rat (make-rat 1 3) (make-rat 1 3)))
