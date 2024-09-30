#lang sicp

(define (check-equal? actual expected)
  (if (equal? actual expected)
      (display "Test passed\n")
      (display "Test failed\n")))

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

(define (make-rectangle-from-segments w l)
    (cons w l)
)

(define (make-rectangle start-point l w)
    (let* ((start-x (x-point start-point))
          (start-y (y-point start-point))
          (end-x ( + start-x w))
          (end-y (+ start-y l))
          (point-w (make-point end-x start-y))
          (point-l (make-point end-x end-y))
         )
        (make-rectangle-from-segments (make-segment start-point point-w) (make-segment point-w point-l))
    )
)

(define (make-rectangle-2 start-point end-point)
    (let* ((start-x (x-point start-point))
          (start-y (y-point start-point))
          (end-x  (x-point end-point))
          (end-y (y-point end-point))
          (point-w (make-point end-x start-y))
          (point-l (make-point end-x end-y))
         )
        (make-rectangle-from-segments (make-segment start-point point-w) (make-segment point-w point-l))
    )
)

(define (rectangle-w rectangle)
    (car rectangle)
)

(define (rectangle-l rectangle)
    (cdr rectangle)
)

(define (segment-length segment)
    (- (y-point (end-segment segment)) (y-point (start-segment segment)))
)

(define (segment-width segment)
    (- (x-point (end-segment segment)) (x-point (start-segment segment)))
)

(define (rectangle-width rectangle)
    (segment-width (rectangle-w rectangle))
)

(define (rectangle-length rectangle)
    (segment-length (rectangle-l rectangle))
)

(define (rectangle-perimeter rectangle)
    (* 2  (+ (rectangle-length rectangle) (rectangle-width rectangle)))
)

(define (rectangle-square rectangle)
    (* (rectangle-length rectangle) (rectangle-width rectangle))
)

(define start-point (make-point 2 4))
(define end-point (make-point 5 6))

(define rectangle-2 (make-rectangle-2 start-point end-point))
(define rectangle (make-rectangle start-point 3 2))

(rectangle-square rectangle)

(check-equal? (rectangle-square rectangle) 6)
(check-equal? (rectangle-perimeter rectangle) 10)
(check-equal? (rectangle-square rectangle-2) 6)
(check-equal? (rectangle-perimeter rectangle-2) 10)

