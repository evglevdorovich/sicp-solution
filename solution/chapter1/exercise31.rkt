;Simpson's Rule for calculating integrals with iterative sum
#lang sicp

(define (prod term a next b)
    (define (iter a result)
        (if (> a b)
        result
        (iter (next a) (* result (term a)))))
    (iter a 1))

(define (next a)
    (+ a 1))

(define (identity a) a)

(define (square a) (* a a))

(define (factorial n)
    (define (next a)
        (+ a 1))
    (prod identity 1 next n)
)

(define (pi n)
    (define (next a)
        (+ a 1 1))
    (* 8 (/ (* (prod square 4 next (+ n 2)) (+ n 2))
        (prod square 3 next (+ n 3))))
        )

(pi 100000)
