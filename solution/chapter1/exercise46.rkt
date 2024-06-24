;iterative improve
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
    (define (improve guess)
        (f guess)
    )
    (define (is-good-enough? guess)
        (let ((next (improve guess)))
            (close-enough? guess next)
        )
    )
((iterative-improve is-good-enough? improve) first-guess))

(define (sqrt x)
    (fixed-point (average-damp (lambda (y) (/ x y)))
    1.0)
)

(define (iterative-improve is-good-enough? improve)
    (lambda (guess)
        (if (is-good-enough? guess)
            guess
            ((iterative-improve is-good-enough? improve) (improve guess))
        )
    )
)

(sqrt 16)
