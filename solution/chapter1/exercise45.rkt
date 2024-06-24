;define nth-root procedure
#lang sicp

(define (compose f g)
(lambda (x) (f (g x)))
)

(define (repeated fun n)
    (if (= n 1)
    fun
    (compose fun (repeated fun (- n 1)))
    )
)

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

(define (nth-root x n)
    (fixed-point
        ((repeated average-damp (+ 1 (floor (log n))))
            (lambda (y) (/ x
                ((repeated (lambda (z) (* z y)) (- n 1)) 1.0)
            ))
         )
    1.0)
)
(nth-root 2 16)
