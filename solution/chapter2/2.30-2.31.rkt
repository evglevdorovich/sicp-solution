#lang sicp

(define (square x) (* x x))

(define (square-tree tree)
    (cond ((pair? tree) (cons (square-tree (car tree)) (square-tree (cdr tree))))
          ((null? tree) nil)
          (else (square tree))
    )
)

(define (square-tree-map tree)
 (map (lambda (sub-tree)
        (if (pair? sub-tree)
            (square-tree-map sub-tree)
            (square sub-tree)
        )
 )
 tree)
)

(define (tree-map fun subject)
     (map (lambda (sub-subject)
            (if (pair? sub-subject)
                (tree-map fun sub-subject)
                (fun sub-subject)
            )
     )
     subject
    )
)

(define (square-tree-abs tree) (tree-map square tree))



(square-tree-map
(list 1
(list 2 (list 3 4) 5)
(list 6 7)))
