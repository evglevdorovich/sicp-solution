;Prod function for calculating pi and factorial iterative + recursive
#lang sicp

(define (prod term a next b)
    (define (iter a result)
        (if (> a b)
        result
        (iter (next a) (* result (term a)))))
    (iter a 1))

(define (prod-rec term a next b)
        (if (> a b)
        1
        (* (term a) (prod-rec term (next a) next b))))

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

(define (pi-rec n)
    (define (next a)
        (+ a 1 1))
    (* 8 (/ (* (prod-rec square 4 next (+ n 2)) (+ n 2))
        (prod-rec square 3 next (+ n 3))))
        )

(pi 1000)
(pi-rec 1000)
