;Accumulate function that works with sum and prod
#lang sicp

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
    (iter a null-value)
        )

(define (accumulate-rec combiner null-value term a next b)
    (if (> a b)
        null-value
        (combiner (term a) (accumulate-rec combiner null-value term (next a) next b))
        ))

(define (next a)
    (+ a 1))

(define (identity a) a)

(define (square a) (* a a))

(define (sum a b) (+ a b))
(define (prod a b) (* a b))

(define (factorial n)
    (define (next a)
        (+ a 1))
    (accumulate sum 0 identity 1 next n)
)

(define (pi n)
    (define (next a)
        (+ a 1 1))
    (* 8 (/ (* (accumulate prod 1 square 4 next (+ n 2)) (+ n 2))
        (accumulate prod 1 square 3 next (+ n 3))))
        )

(define (pi-rec n)
    (define (next a)
        (+ a 1 1))
    (* 8 (/ (* (accumulate-rec prod 1 square 4 next (+ n 2)) (+ n 2))
        (accumulate-rec prod 1 square 3 next (+ n 3))))
        )

(pi 10000)
(pi-rec 10000)
