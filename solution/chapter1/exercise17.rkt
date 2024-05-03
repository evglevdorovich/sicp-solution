;multiplication with log(n) and recursive process
#lang sicp

(define (* a b)
    (define (even? n)
        (= (remainder n 2) 0))

    (define (double n)
        (+ n n))

    (define (halve n)
        (/ n 2))

        (cond ((= b 0) 0)
              ((even? b) (* (double a) (halve b)))
              (else (+ a (* a (- b 1)))))

)
