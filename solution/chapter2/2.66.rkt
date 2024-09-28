#lang sicp

(define (entry tree) (car tree))
(define (key tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
(list entry left right))

(define (lookup given-key set-of-records)
    (if (null? set-of-records) #f
        (let ((x (key (entry set-of-records))))
            (cond ((> x given-key) (lookup given-key (left-branch set-of-records)))
                ((< x given-key) (lookup given-key (branch-branch set-of-records)))
                (else (key set-of-records))
            )
        )
    )
)
