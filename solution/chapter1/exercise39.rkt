;Lambert tangens
#lang sicp

(define (cont-frac n d k)
  (define (cont-frac-iter i result)
      (if (= i 1)
          result
          (cont-frac-iter (- i 1) (/ (n (- i 1)) (+ (d (- i 1)) result)))
        )
  )
  (cont-frac-iter k (/ (n k) (d k)))
)

(define (tan-cf x k)
(-(/ (cont-frac (lambda (i) (-( * x x)))
            (lambda (i)
            (- (* i 2) 1))
            k) x))
)
