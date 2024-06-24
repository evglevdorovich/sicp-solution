#lang sicp

(define (print-point p)
    (newline)
    (display "(")
    (display (x-point p))
    (display ",")
    (display (y-point p))
    (display ")")
)

(define (make-point x y)
    (cons x y)
)

(define (x-point point)
    (car point)
)

(define (y-point point)
    (cdr point)
)

(define (make-segment start end)
    (cons start end)
)

(define (start-segment segment)
    (car segment)
)

(define (end-segment segment)
    (cdr segment)
)

(define (avg a b)
    (/ (+ a b) 2)
)

(define (midpoint start-point end-point)
    (let (
        (x1 (x-point start-point))
        (x2 (x-point end-point))
        (y1 (y-point start-point))
        (y2 (y-point end-point))
        )
    (make-point (avg x1 x2) (avg y1 y2))
    )
)

(define (midpoint-segment segment)
    (midpoint (start-segment segment) (end-segment segment))
)

(midpoint-segment (make-segment (make-point 2 4) (make-point 4 8)))



