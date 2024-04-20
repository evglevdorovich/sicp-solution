#lang sicp
; calculates cube root for a number

(define (cube-root-iter guess x)
(if (good-enough? guess x)
    guess
(cube-root-iter (improve guess x) x)))

(define (improve guess x)
  (/  (+ (/ x (square guess))
                      (* 2 guess))
                   3)
                  )

(define (average x y)
(/ (+ x y) 2))

(define (good-enough? guess x)
(< (abs (- (improve guess x) guess))
(* guess 0.001)))

(define (square x)
  (* x x))

(define (cube x)
  (* x x x))

(define (cube-root x)
(cube-root-iter 1.0 x))

(cube (cube-root 333))
