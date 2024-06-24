#lang sicp
;calculates pascal triangle with the recursive process

(define (pasc-triangle row col)
    (define (pasc-triangle-rec row-curr col-curr)
    (cond ((or(= row-curr 0) (= col-curr 0) (= row-curr col-curr)) 1)
          ((or(< row-curr 0) (< col-curr 0) (> col-curr row)) 0)
          (else (+ (pasc-triangle-rec (- row-curr 1) col-curr)
          (pasc-triangle-rec (- row-curr 1) (- col-curr 1))))
    ))
    (pasc-triangle-rec row col)
)

(pasc-triangle 8 6)
