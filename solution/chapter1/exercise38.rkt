;De Fractionibus Continuis e - 2
#lang sicp

(define (cont-frac-iter n d k)
  (define (cont-frac-iter i result)
      (if (= i 1)
          result
          (cont-frac-iter (- i 1) (/ (n (- i 1)) (+ (d (- i 1)) result)))
        )
  )
  (cont-frac-iter k (/ (n 1) (d 1)))
)

(cont-frac (lambda (i) 1.0)
            (lambda (i)
            (if (= (remainder i 3) 2)
                    (* (/ (+ i 1) 3) 2)
                    1))
            10)
