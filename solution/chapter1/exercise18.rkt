;multiplication with log(n) and interative process
#lang racket/base
(require racket/trace)

(define (* a b)
    (define (even? n)
        (= (remainder n 2) 0))

    (define (double n)
        (+ n n))

    (define (halve n)
        (/ n 2))

    (define (*-iter a b result)
        (cond ((= b 0) result)
              ((even? b) (*-iter (double a) (halve b) result))
              (else (*-iter a (- b 1) (+ a result))))
    )
    (*-iter a b 0)

)

(trace *)
(* 22 34)
