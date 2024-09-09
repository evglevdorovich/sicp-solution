#lang sicp

(#%require sicp-pict)

(define wave
   (segments->painter
        (list
               (make-segment (make-vect .25 0) (make-vect .35 .5))
               (make-segment (make-vect .35 .5) (make-vect .3 .6))
               (make-segment (make-vect .3 .6) (make-vect .15 .4))
               (make-segment (make-vect .15 .4) (make-vect 0 .65))
               (make-segment (make-vect 0 .65) (make-vect 0 .85))
               (make-segment (make-vect 0 .85) (make-vect .15 .6))
               (make-segment (make-vect .15 .6) (make-vect .3 .65))
               (make-segment (make-vect .3 .65) (make-vect .4 .65))
               (make-segment (make-vect .4 .65) (make-vect .35 .85))
               (make-segment (make-vect .35 .85) (make-vect .4 1))
               (make-segment (make-vect .4 1) (make-vect .6 1))
               (make-segment (make-vect .6 1) (make-vect .65 .85))
               (make-segment (make-vect .4 .75) (make-vect .45 .70))  ;here
               (make-segment (make-vect .45 .70) (make-vect .55 .70))
               (make-segment (make-vect .55 .70) (make-vect .60 .75))
               (make-segment (make-vect .65 .85) (make-vect .6 .65))
               (make-segment (make-vect .6 .65) (make-vect .75 .65))
               (make-segment (make-vect .75 .65) (make-vect 1 .35))
               (make-segment (make-vect 1 .35) (make-vect 1 .15))
               (make-segment (make-vect 1 .15) (make-vect .6 .45))
               (make-segment (make-vect .6 .45) (make-vect .75 0))
               (make-segment (make-vect .75 0) (make-vect .6 0))
               (make-segment (make-vect .6 0) (make-vect .5 .3))
               (make-segment (make-vect .5 .3) (make-vect .4 0))
               (make-segment (make-vect .4 0) (make-vect .25 0))
        )
   )
)

(define (split f1 f2)
    (lambda (painter n) (if (= n 0) painter
    (let ((smaller ((split f1 f2) painter (- n 1))))
        (f1 painter (f2 smaller smaller))
    ))
))

(define right-split (split beside below))
(define up-split (split below beside))

(define (corner-split painter n)
(if (= n 0) painter
    (let ((up (up-split painter (- n 1)))
    (right (right-split painter (- n 1))))
        (let ((top-left up)
        (bottom-right right)
        (corner (corner-split painter (- n 1))))
        (beside (below painter top-left)
        (below bottom-right corner)))
    )
)
)

(define (square-of-four tl tr bl br)
    (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
    (bottom (beside (bl painter) (br painter))))
    (below bottom top)))
)

(define (rotate90 painter)
(transform-painter painter
(make-vect 1.0 0.0)
(make-vect 1.0 1.0)
(make-vect 0.0 0.0)))

(define (square-limit painter n)
    (let ((quarter (corner-split painter n)))
        ((square-of-four flip-vert rotate180 identity flip-horiz) quarter)
    )
)

(paint wave)
(paint (corner-split einstein 4))
(paint (square-limit einstein 4))
