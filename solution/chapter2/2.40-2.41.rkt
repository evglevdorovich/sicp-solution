#lang sicp

(define (square n)
  (* n n))

(define (prime? n)
    (and(= n (smallest-divisor n))
    (not (= n 1)))
)

(define (enumerate-interval low high)
    (if (> low high) nil
        (cons low (enumerate-interval (+ 1 low) high))
    )
)

(define (filter predicate sequence)
    (if (null? sequence) nil
    (if (predicate (car sequence))
    (cons (car sequence) (filter predicate (cdr sequence)))
    (filter predicate (cdr sequence))
    ))
)

(define (accumulate op initial sequence)
    (if (null? sequence) initial
        (op (car sequence)
        (accumulate op initial (cdr sequence))0
        )
    )
)

(define (divides? a b) (= (remainder b a) 0))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (flatmap proc seq)
    (accumulate append nil (map proc seq))
)

(define (prime-sum? pair)
    (prime? (+ (car pair) (cadr pair)))
)

(define (make-pair-sum pair)
    (list (car pair) (cadr pair) (+ (car pair) (cadr pair)))
)

;here
(define (unique-pairs n)
    (flatmap (lambda (i)
                            (map (lambda (j) (list i j))
                                (enumerate-interval 1 (- i 1))
                            )
                        ) (enumerate-interval 1 n))
)

(define (unique-triples n)
    (flatmap (lambda (i)
                            (flatmap (lambda (j)
                                 (map (lambda (k) (list i j k))
                                    (enumerate-interval 1 (- j 1))
                                 )
                                 )
                                    (enumerate-interval 1 (- i 1))
                            )
                        ) (enumerate-interval 1 n))
)

(define (make-triple-sum n sum)
    (let ((triples (unique-triples n)))
        (filter (lambda (triple) (= sum (+ (car triple) (cadr triple) (caddr triple)))) triples)
    )
)
