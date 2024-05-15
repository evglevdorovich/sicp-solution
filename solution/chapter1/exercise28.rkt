;fast-prime - Miller-Rabin test Carmichael numbers
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
(if (fast-prime? n 5)
    (report-prime (- (runtime) start-time))))

(define (report-prime elapsed-time)
    (display " *** ")
    (display elapsed-time)
  (display "\n"))

(define (smallest-divisor n) (find-divisor n 2))

(define (prime? n)
    (= n (smallest-divisor n)))

(define (even? n)
    (= (remainder n 2) 0))

(define (fermat-test n)
    (define (try-it a)
        (define result (expmod a (- n 1) n))
        (and (= result (remainder 1 n)) (not (= result 0))))
    (try-it (+ 1 (random (- n 1)))))

(define (expmod base exp m)
    (cond ((= exp 0) 1)
    ((even? exp)
        (if (non-trivial? base exp) 0
        (remainder(square (expmod base (/ exp 2) m))m)))
    (else
        (remainder(* base (expmod base (- exp 1) m))m))))

(define (non-trivial? numb n)
    (if (and (or (not (= numb 1)) (not (= numb (- n 1)))) (= (remainder 1 n) (square numb))) true)
false)

(define (fast-expt b n)
    (cond ((= n 0) 1)
    ((even? n) (square (fast-expt b (/ n 2))))
(else (* b (fast-expt b (- n 1))))))

(define (fast-prime? n times)
    (cond ((= times 0) true)
    ((fermat-test n) (fast-prime? n (- times 1)))
(else false)))

(define (search-for-primes min max count)
    (cond ((even? min) (iter-search-for-primes (+ min 1) max count))
          (else (iter-search-for-primes (+ min 2) max count))))

; assumed that always reached count
(define (iter-search-for-primes min max count)
    (define start-time (runtime))
    (if (and (< min max) (> count 0))
        (cond ((fast-prime? min 50)
            (display min)
            (report-prime (- (runtime) start-time))
            (iter-search-for-primes (+ min 2) max (- count 1)))
        (else (iter-search-for-primes (+ min 2) max count))))
)

(search-for-primes 6600 7600 5)
