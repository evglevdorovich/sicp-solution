;continued fraction with recursive and iterative process
#lang sicp

(define (cont-frac n d k)
  (if (= k 1)
    (/ (n k) (d k))
    (/ (n k) (+ (d k) (cont-frac n d (- k 1))))
  ))

(define (cont-frac-iter n d k)
  (define (cont-frac-iter i result)
      (if (= i 1)
          result
          (cont-frac-iter (- i 1) (/ (n (- i 1)) (+ (d (- i 1)) result)))
        )
  )
  (cont-frac-iter k (/ (n 1) (d 1)))
)

(cont-frac-iter (lambda (i) 1.0)
            (lambda (i) 1.0)
            12)

(cont-frac (lambda (i) 1.0)
            (lambda (i) 1.0)
            12)

;0.6180
