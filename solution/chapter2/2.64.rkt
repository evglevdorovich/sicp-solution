#lang sicp

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
(list entry left right))

(define (list->tree els)
    (partial-tree els (length els))
)

; 1 3 | 5 | 7 9 11
(define (partial-tree els n)
    (if (= n 0) (cons '() els)
        (let ((left-size (quotient (- n 1) 2)))
             (let ((left-tree (partial-tree els left-size)))
                (let ((left-els (car left-tree))
                     (non-left-els (cdr left-tree)))
                     (let ((this-entry (car non-left-els))
                            (right-els (cdr non-left-els))
                            (right-size (- n (+ left-size 1)))
                            )
                            (let ((right-tree (partial-tree right-els right-size)))
                                (let ((right-els (car right-tree))
                                      (unprocessed-els (cdr right-tree))
                                )
                                    (cons (make-tree this-entry
                                               left-els
                                               right-els
                                    ) unprocessed-els

                                    )
                                )
                            )
                     )

                )
             )

        )
    )
)


(list->tree (list 1 3 5 7 9 11))