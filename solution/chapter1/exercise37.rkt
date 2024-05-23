;continued fraction with recursive and iterative process
#lang sicp


(define (cont-frac n d k)
  (define (cont-frac-rec i)
  (if (= i k)
      (/ (n i) (d i))
      (/ (n i) (+ (d i) (cont-frac-rec n d (+ i 1))))
    )
 )
 (cont-frac-rec 1)
)

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
