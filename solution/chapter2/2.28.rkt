#lang sicp

(define (fringe items)
    (cond
        ((eq? '() items) items)
        ((not (pair? items)) (list items))
        (else
            (append (fringe (car items)) (fringe (cdr items)))
        )
    )
)


(define x (list (list 1 2) (list 3 4)))
(fringe (list x x))
(fringe x)


