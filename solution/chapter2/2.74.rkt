#lang racket


(define (get-record departament employee)
    ((get 'get-record departament) employee)
)

(define (get-salary department employee)
    ((get 'get-salary deparment) employee)
)

(define (find-employee-record departments employee)
    (if (null? departments) false
        (let ((found ((get 'get-record (car departments))employee)))
            (if (= false found) (find-employee-record (cdr departments) employee)
                found
            )
        )
    )
)
