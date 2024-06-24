;double function
#lang sicp

(define (double fun)
(lambda (x) (fun (fun x)))
)

(((double (double double)) inc) 5)
