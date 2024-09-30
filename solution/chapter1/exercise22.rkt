;find first n-prime numbers
#lang sicp

(define (square n)
  (* n n))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (timed-prime-test n)
    (newline)
    (display n)
    (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
(if (prime? n)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (even? n)
    (= (remainder n 2) 0))

(define (search-for-primes min max count)
    (cond ((even? min) (iter-search-for-primes (+ min 1) max count))
          (else (iter-search-for-primes (+ min 2) max count))))

; assumed that always reached count
(define (iter-search-for-primes min max count)
    (define start-time (runtime))
    (if (and (< min max) (> count 0))
        (cond ((prime? min)
            (report-prime (- (runtime) start-time))
            (iter-search-for-primes (+ min 2) max (- count 1)))
        (else (iter-search-for-primes (+ min 2) max count))))
)
