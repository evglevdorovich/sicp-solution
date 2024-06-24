;find x^x = 1000 with fix-point function (and average dumping for comparison)
#lang sicp

(define (average x y)
  (/ (+ x y) 2))

(define tolerance 0.00001)
(define (fixed-point f first-guess)
    (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
    (define (try guess)
    (let ((next (f guess)))
        (display next)
        (display " \n")
        (if (close-enough? guess next)
        next
        (try next))))
(try first-guess))

;normal
;(fixed-point (lambda (x) (/ (log 1000) (log x))) 2)
;35 times vs 10

;average-dumping
(fixed-point (lambda (x) (average x (/ (log 1000) (log x)))) 2)
