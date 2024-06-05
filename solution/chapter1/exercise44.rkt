#lang sicp

(define (square x)
  (* x x))

(define (compose f g)
(lambda (x) (f (g x)))
)

(define (repeated fun n)
    (if (= n 1)
    fun
    (compose fun (repeated fun (- n 1)))
    )
)

(define dx 0.00001)

(define (avg a b c)
    (/ (+ a b c) 3)
)

(define (smooth f)
    (lambda (x) (avg (f x) (f (+ x dx)) (f (- x dx))))
)

(define (smooth-nth f n)
    ((repeated smooth n) f)
)

((smooth-nth square 5) 5)
