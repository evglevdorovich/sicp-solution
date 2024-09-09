#lang sicp

(#%require sicp-pict)

(define (transform-painter painter origin corner1 corner2)
    (lambda (frame)
        (let ((m (frame-coord-map frame)))
            (let ((new-origin (m origin)))
            (painter
                (make-frame new-origin (vector-sub (m corner1) new-origin) (vector-sub (m corner2) new-origin)))
            )
        )
    )
)

(define (flip-horiz painter)
    (transform-painter painter
                (make-vect 1.0 0.0)
                (make-vect 0.0 0.0)
                (make-vect 1.0 1.0)
                )
)

(define (flip-180 painter)
    (transform-painter painter
                (make-vect 1.0 1.0)
                (make-vect 0.0 1.0)
                (make-vect 1.0 0.0)
                )
)

(define (flip-270 painter)
    (transform-painter painter
                (make-vect 0.0 1.0)
                (make-vect 0.0 0.0)
                (make-vect 1.0 1.0)
                )
)

(define (below painter1 painter2)
    (let ((split-point (make-vect 0.0 0.5)))
        (let ((paint-below
        (transform-painter
        painter1
        (make-vect 0.0 0.0)
        (make-vect 1.0 0.0)
        split-point))
        (paint-above
        (transform-painter
        painter2
        split-point
        (make-vect 1.0 0.5)
        (make-vect 0.0 1.0))))
        (lambda (frame)
        (paint-below frame)
        (paint-above frame))
        )
    )
)

(define (beside painter1 painter2)
(let ((split-point (make-vect 0.5 0.0)))
(let ((paint-left
(transform-painter
painter1
(make-vect 0.0 0.0)
split-point
(make-vect 0.0 1.0)))
(paint-right
(transform-painter
painter2
split-point
(make-vect 1.0 0.0)
(make-vect 0.5 1.0))))
(lambda (frame)
(paint-left frame)
(paint-right frame)))))

(define (below-from-beside painter1 painter2)
    (flip-180 (flip-270 (beside (flip-270 painter1) (flip-270 painter2))))
)

(paint (below einstein einstein))
(paint (below-from-beside einstein einstein))
(paint (beside einstein einstein))
