;define procedure cubic
#lang sicp

(define (square n)
        (* n n))

(define (average x y)
  (/ (+ x y) 2))

(define (average-damp f)
  (lambda (x) (average x (f x))))

(define tolerance 0.00001)
(define dx 0.00001)

(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
    (define (try guess)
    (let ((next (f guess)))
        (if (close-enough? guess next)
        next
        (try next))))
(try first-guess))

;(define (sqrt x)
;    (fixed-point (average-damp (lambda (y) (/ x y)))
;    1.0)
;)

(define (deriv g)
    (lambda (x) (/ ( - (g (+ x dx)) (g x)) dx))
)

(define (cube x) (* x x x))

(define (newton-transform g)
    (lambda (x) (- x (/ (g x) ((deriv g) x))))
)

(define (newtons-method g guess)
    (fixed-point (newton-transform g) guess)
)

(define (sqrt x)
    (newtons-method
    (lambda (y) (- (square y) x))
    1.0)
)

(define (fixed-point-of-transform g transform guess)
    (fixed-point (transform g) guess)
)

(define (cubic a b c)
    (lambda (x) (+ (cube x) (* a (square x)) (* b x) c))
)

(define a 1)
(define b 2)
(define c 3)
(newtons-method (cubic a b c) 1)


