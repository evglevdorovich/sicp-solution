#lang sicp
; exercise 1.11 recursive function
; f(n)=f(n−1)+2f(n−2)+3f(n−3) if n>= 3
; n if n < 3

(define (exerc-fun n)
  (cond((< n 3) n)
  (else (+ (exerc-fun (- n 1))
            (* 2 (exerc-fun (- n 2)))
            (* 3 (exerc-fun (- n 3))))))
)

(exerc-fun 5)
