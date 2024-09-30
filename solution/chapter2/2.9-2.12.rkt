#lang sicp

(define (add-interval x y)
    (make-interval (+ (lower-bound x) (lower-bound y))
    (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
    (let ((l1 (lower-bound x))
        (u1 (upper-bound x))
        (l2 (lower-bound y))
        (u2 (upper-bound y)))
        (cond ((and (<= l1 0) (<= u1 0) (>= l2 0) (>= u2 0))
            (make-interval (* l1 u2) (* u1 l2)))
            ((and (<= l1 0) (<= u1 0) (<= l2 0) (>= u2 0))
                (make-interval (* l1 u2) (* l1 l2)))
            ((and (<= l1 0) (<= u1 0) (<= l2 0) (<= u2 0))
                    (make-interval (* u1 u2) (* l1 l2)))
            ((and (>= l1 0) (>= u1 0) (<= l2 0) (<= u2 0))
                        (make-interval (* l2 u1) (* l1 u2)))
            ((and (>= l1 0) (>= u1 0) (>= l2 0) (>= u2 0))
                        (make-interval (* l1 l2) (* u1 u2)))
            ((and (>= l1 0) (>= u1 0) (<= l2 0) (>= u2 0))
                        (make-interval (* u1 l2) (* u1 u2)))
            ((and (<= l1 0) (>= u1 0) (<= l2 0) (<= u2 0))
                        (make-interval (* u1 l2) (* l1 l2)))
            ((and (<= l1 0) (>= u1 0) (>= l2 0) (>= u2 0))
                        (make-interval (* l1 u2) (* u1 u2)))
            (else (make-interval (min (* l1 u2) (* u1 l2))
                      (max (* l1 l2) (* u1 u2))))
        )
    )
)



(define (div-interval x y)
    (if (spans-zero? y)
    (error "division by zero")
    (mul-interval
    x
    (make-interval (/ 1.0 (upper-bound y))
(/ 1.0 (lower-bound y))))))

(define (make-interval a b) (cons a b))

(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (spans-zero? interval)
    (and (<= (lower-bound interval) 0) (>= (upper-bound interval) 0))
)

(define (sub-interval x y)
    (let ((p1 (upper-bound y))
        (p2 (lower-bound y))
        (p3 (lower-bound x))
        (p4 (upper-bound x)))
        (make-interval (- p3 p1) (- p4 p2))
    )
)

(define (check f1 f2)
    (if (= f1 f2)
        (display " equals ")
        (display " not equals ")
    )
)

(define (make-center-width c w)
    (make-interval (- c w) (+ c w)))

(define (center i)
(/ (+ (lower-bound i) (upper-bound i)) 2))

(define (width i)
(/ (- (upper-bound i) (lower-bound i)) 2))

(define (percent i)
    (* 100 (/ (width i) (center i)))
)

(define (to-width center percent)
    (/ (* percent center) 100)
)

(define (make-center-percent center percent)
    (make-center-width center (to-width center percent))
)

(define interval1 (make-interval 1 2))
(define interval2 (make-interval 2 3))

(check (width (add-interval interval1 interval2)) (+ (width interval1) (width interval2)))
(check (width (sub-interval interval1 interval2)) (+ (width interval2) (width interval1)))
(check (width (mul-interval interval1 interval2)) (* (width interval2) (width interval1)))
(check (width (div-interval interval1 interval2)) (/ (width interval2) (width interval1)))


