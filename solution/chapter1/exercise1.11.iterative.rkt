#lang sicp
; exercise 1.11 iterative function from recursive
; f(n)=f(n−1)+2f(n−2)+3f(n−3) if n>= 3
; n if n < 3

(define (exerc-fun n)
    (define (iter a b c counter)
        (cond ((> counter n) a)
           (else (iter (+ a (* 2 b) (* 3 c))
                        a
                        b
                        (+ 1 counter)))))
    (if (< n 3) n
                (iter 2 1 0 3))
)


(exerc-fun 5)
