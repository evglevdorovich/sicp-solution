;accumulate-filtered squares of the prime numbers in the interval a - b
;the product of all the positive integers less than n that are relatively prime to n
#lang sicp

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
    (and(= n (smallest-divisor n))
    (not (= n 1)))
)

(define (even? n)
    (= (remainder n 2) 0))

(define (filtered-accumulate predicate combiner null-value term a next b)
    (define (iter a result)
        (if (> a b)
        result
        (iter (next a)
        (if (predicate a)
        (combiner result (term a))
        result
        ))
        )
    )
    (iter a null-value)
        )

(define (next a)
    (+ a 1))

(define (identity a) a)

(define (square a) (* a a))

(define (sum a b) (+ a b))
(define (prod a b) (* a b))

(define (gcd a b)
    (if (= b 0)
    a
    (gcd b (remainder a b)))
)

(define (sum-prime-squares a b)
    (filtered-accumulate prime? sum 0 square a next b)
)

(define (prod-relatively-prime n)
    (define (relative-prime i)
    (= (gcd i n) 1))
    (filtered-accumulate relative-prime prod 1 identity 1 next n)
)

(sum-prime-squares 1 10)

(prod-relatively-prime 10)
