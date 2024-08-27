#lang racket/base
 (require racket/trace)

 (define (enumerate-interval low high)
     (if (> low high) null
         (cons low (enumerate-interval (+ 1 low) high))
     )
 )

 (define (filter predicate sequence)
     (if (null? sequence) null
     (if (predicate (car sequence))
     (cons (car sequence) (filter predicate (cdr sequence)))
     (filter predicate (cdr sequence))
     ))
 )

 (define (accumulate op initial sequence)
     (if (null? sequence) initial
         (op (car sequence)
         (accumulate op initial (cdr sequence))
         )
     )
 )

 (define (flatmap proc seq)
     (accumulate append null (map proc seq))
 )

 (define empty-board `())

 (define (queens board-size)
     (define (queen-cols k)
         (if (= k 0)
         (list empty-board)
     (filter
         (lambda (positions) (safe? k positions))
         (flatmap
             (lambda (rest-of-queens)
             (map (lambda (new-row)
                 (adjoin-position
                     new-row k rest-of-queens))
             (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1)))
                 )))
         (queen-cols board-size))

 ;there

 (define (adjoin-position new-row k rest-of-queens)
     (cons (cons k new-row) rest-of-queens)
 )

 (define (safe? k positions)
     (let ((k-pos (car (filter (lambda (pos) (= (car pos) k)) positions))))
         (accumulate (lambda (a b) (and a b)) #t (map (lambda (pos)
             (let ((col-diff (- (car k-pos) (car pos)))
                  (row-diff (- (cdr k-pos) (cdr pos))))
                  (not (and (not (eq? k-pos pos))
                      (or
                          (= (cdr pos) (cdr k-pos))
                          (= col-diff (abs row-diff))
                      )
                  ))
             )
         ) positions))

     )
 )

