;Simpson's Rule for calculating integrals with iterative sum
#lang sicp

(define (integral f a b dx)
    (define (add-dx x)
        (+ x dx))
    (* (sum f (+ a (/ dx 2.0)) add-dx b)dx))

(define (cube a)
  (* a a a)
)

(define (sum term a next b)
    (define (iter a result)
        (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
    (iter a 0))

;dx 0.001 = 0.249999875000001
;dx 0.01 = 0.24998750000000042

(define (even? a)
    = (remainder a 2) 0)

(define (simp-integral f a b n)
    (define dx
        (/ (- b a) n))
    (define (next x)
        (+ x 2))
    (define (fun i)
        (+ (* 2 (f (+ (* i dx) a))) (* 4 (f (+ (* (+ i 1) dx) a)))))
    (* (/ dx 3) (- (+ (sum fun 0 next (- n 1)) (f (+ a (* n dx)))) a))
)

(simp-integral cube 0 1 10000000)

(integral cube 0 1 0.001)
