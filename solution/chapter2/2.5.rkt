;2^a 3^b
#lang sicp

(define (even? n)
    (= (remainder n 2) 0))

(define (cons x y)
    (* (expt 2 x) (expt 3 y))
)

(define (car z)
    (define (div-2 el result)
        (if (= (remainder el 2) 0)
            (div-2 (/ el 2) (+ result 1))
            result
        )
    )
    (div-2 z 0)
)

(define (cdr z)
    (define (div-3 el result)
        (if (= (remainder el 3) 0)
            (div-3 (/ el 3) (+ result 1))
            result
        )
    )
    (div-3 z 0)
)

; from 1.16
(define (expt b n)

    (define (square n)
        (* n n))

    (define (expt-iter b counter product)
        (cond ((= counter 0) product)
              ((even? counter) (expt-iter (square b) (/ counter 2) product))
              (else (expt-iter b (- counter 1) (* product b)))))

  (expt-iter b n 1)

)
;

(car (cons 0 2))
