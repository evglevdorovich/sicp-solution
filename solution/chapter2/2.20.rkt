#lang sicp
(define (even? n)
        (= (remainder n 2) 0)
)

(define (odd? n)
        (= (remainder n 2) 1)
)

(define (extract items condition)
        (define (extract-iter items result)
            (if (null? items) result
                (if (condition (car items))
                    (extract-iter (cdr items) (append result (list (car items))))
                    (extract-iter (cdr items) result)
                )
            )
        )
    (extract-iter items (list ))
)


(define (same-parity first . l)
    (if (even? first)
            (append (list first) (extract l even?))
            (append (list first) (extract l odd?))
    )
)

(same-parity 1 2 3 4 5 6 7)
