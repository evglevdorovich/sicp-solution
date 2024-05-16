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

(define (factorial n)
    (define (next a)
        (+ a 1))
    (prod identity 1 next n)
)

(factorial 5)
