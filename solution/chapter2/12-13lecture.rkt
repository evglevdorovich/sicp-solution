#lang sicp

(define make-tree cons)
(define datum car)
(define children cdr)
(define (leaf? node)
    (null? (children node))
)

(define (treemap fn tree)
    (if (null? tree) `()
        (cons (fn (car tree))
            (treemap fn (cdr tree))
        )
    )
)

(define (deepmap fn lol)
    (if (not (list? lol)) (fn lol)
        (map (lambda (el) (deepmap fn el)) lol)
    )
)

(define (depth-first-search tree)
    (print (datum tree))
    (for-each depth-first-search (children tree))
)

(define (breadth-first-search tree)
    (bfs-iter (list tree))
)

(define (bfs-iter queue)
    (if (null? queue)
        `done
        (let ((task (car queue)))
            (print (datum task))
            (bfs-iter (append (cdr queue) (children task)))
        )
    )
)
