
#lang racket/base
(require racket/trace)

(define (expt-recurs b n)
  (if (= n 0)
      1
      (* b (expt-recurs b (- n 1)))))

(trace expt-recurs)
(expt-recurs 9 7)
