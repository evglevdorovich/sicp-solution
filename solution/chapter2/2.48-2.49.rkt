#lang sicp

(define (frame-coord-map frame)
    (lambda (v)
    (add-vect (origin-frame frame)
    (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
    (scale-vect (ycor-vect v) (edge2-frame frame))))
))

(define (make-vect x y)
    (cons x y)
)

(define (xcor-vect vect)
    (car vect)
)

(define (ycor-vect vect)
    (cdr vect)
)

(define (add-vect v1 v2)
    (make-vect (+ (xcor-vect v1) (xcor-vect v2))
               (+ (ycor-vect v1) (ycor-vect v2))
    )
)

(define (sub-vect v1 v2)
    (make-vect (- (xcor-vect v1) (xcor-vect v2))
               (- (ycor-vect v1) (ycor-vect v2))
    )
)

(define (scale-vect s v)
    (make-vect (* (xcor-vect v) s) (* (ycor-vect v) s))
)
(define (make-segment v1 v2)
    (make-vect v1 v2)
)

(define (start-segment seg)
    (car seg)
)

(define (end-segment seg)
    (cdr seg)
)

(define (segments->painter segment-list)
    (lambda (frame)
        (for-each
            (lambda (segment)
            (draw-line
                ((frame-coord-map frame)
                (start-segment segment))
                ((frame-coord-map frame)
                (end-segment segment)))
            )
        segment-list)
        )
)

(define (draw-outline frame)
    (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 0))
                             (make-segment (make-vect 0 0) (make-vect 0 1))
                             (make-segment (make-vect 0 1) (make-vect 1 1))
                             (make-segment (make-vect 1 0) (make-vect 1 1))
    ))
)

(define (draw-x frame)
    (segments->painter (list (make-segment (make-vect 0 0) (make-vect 1 1))
                             (make-segment (make-vect 1 0) (make-vect 0 1))
    ))
)

(define (draw-diamond frame)
    (segments->painter (list (make-segment (make-vect 0.5 0) (make-vect 0 0.5))
                             (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
                             (make-segment (make-vect 0 0.5) (make-vect 0.5 1))
                             (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
    ))
)

(define wave-segments
 (list
  (make-segment
   (make-vect 0.006 0.840)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.006 0.635)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.155 0.591))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.155 0.392))
  (make-segment
   (make-vect 0.304 0.646)
   (make-vect 0.403 0.646))
  (make-segment
   (make-vect 0.298 0.591)
   (make-vect 0.354 0.492))
  (make-segment
   (make-vect 0.403 0.646)
   (make-vect 0.348 0.845))
  (make-segment
   (make-vect 0.354 0.492)
   (make-vect 0.249 0.000))
  (make-segment
   (make-vect 0.403 0.000)
   (make-vect 0.502 0.293))
  (make-segment
   (make-vect 0.502 0.293)
   (make-vect 0.602 0.000))
  (make-segment
   (make-vect 0.348 0.845)
   (make-vect 0.403 0.999))
  (make-segment
   (make-vect 0.602 0.999)
   (make-vect 0.652 0.845))
  (make-segment
   (make-vect 0.652 0.845)
   (make-vect 0.602 0.646))
  (make-segment
   (make-vect 0.602 0.646)
   (make-vect 0.751 0.646))
  (make-segment
   (make-vect 0.751 0.646)
   (make-vect 0.999 0.343))
  (make-segment
   (make-vect 0.751 0.000)
   (make-vect 0.597 0.442))
  (make-segment
   (make-vect 0.597 0.442)
   (make-vect 0.999 0.144))))
