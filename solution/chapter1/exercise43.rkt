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

((repeated square 4) 5)
