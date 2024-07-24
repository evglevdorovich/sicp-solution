#lang sicp

(define (make-mobile left right)
    (list left right)
)


(define (make-branch length structure)
    (list length structure)
)

(define (left-branch mobile)
    (car mobile)
)

(define (right-branch mobile)
    (cadr mobile)
)

(define (branch-length branch)
    (car branch)
)

(define (branch-structure branch)
    (cadr branch)
)

(define (total-weight mobile)
    (if (not (pair? mobile)) mobile
        (+ (total-weight (branch-structure (left-branch mobile)))
            (total-weight (branch-structure (right-branch mobile)))
        )
    )
)

(define (mobile-balanced? mobile)
    (let ((left (branch-structure (left-branch mobile)))
        (right (branch-structure (right-branch mobile)))
    )
        (if (or (not (pair? left)) (not (pair? right)))
            (= (* (total-weight left) (branch-length (left-branch mobile)))
                (* (total-weight right) (branch-length (right-branch mobile)))
            )
            (and (mobile-balanced? left) (mobile-balanced? right))
        )
    )
)

(define nested-left-branch (make-branch 4 5))

(define nested-right-branch (make-branch nested-left-branch (make-branch 6 7)))

(define left (make-branch 1 2))

(define right (make-branch 3 nested-right-branch))

(define mobile (make-mobile left right))

(define mobile2 (make-mobile (make-branch 5 2)
                            (make-branch 1 10)))

 (define m1 (make-mobile
             (make-branch 4 6)
             (make-branch 5
                          (make-mobile
                           (make-branch 3 7)
                           (make-branch 9 8)))))

 (define m2 (make-mobile
             (make-branch 4 6)
             (make-branch 2
                          (make-mobile
                           (make-branch 5 8)
                           (make-branch 10 4)))))

(mobile-balanced? m1)
(mobile-balanced? m2)


