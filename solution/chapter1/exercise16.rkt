;iterative exponential process with 0(1) space
#lang sicp

(define (expt b n)
    (define (even? n)
        (= (remainder n 2) 0))

    (define (square n)
        (* n n))

    (define (expt-iter b counter product)
        (cond ((= counter 0) product)
              ((even? counter) (expt-iter (square b) (/ counter 2) product))
              (else (expt-iter b (- counter 1) (* product b)))))

  (expt-iter b n 1)

)
