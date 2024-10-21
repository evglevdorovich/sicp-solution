#lang racket

(define (deriv exp var)
    (cond ((number? exp) 0)
          ((variable? exp) (if (same-variable? exp var) 1 0))
          ((sum? exp) (make-sum (deriv (addend exp) var)
                                (deriv (augend exp) var)))
          ((product? exp)
            (make-sum
               (make-product (multiplier exp)
                             (deriv (multiplicand exp) var))
               (make-product (deriv (multiplier exp) var)
                                    (multiplicand exp))))
          ((exponentiation? exp)
            (make-product
                (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp))
            )
          )
    (else
        (error "unknown expression type: DERIV" exp))
    )
)

(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
(and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else (list a1 '+ a2)))
)

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else (list m1 '* m2))
    )
)

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (car s))

(define (augend s)
    (if (null? (cdddr s)) (caddr s)
        (make-op (cddr s))
    )
)

(define (make-op exp)
    (cond
          ((number? exp) exp)
          ((sum? exp) (make-sum (addend exp) (augend exp)))
          ((product? exp)
              (if (null? (cdddr exp))
                (make-product (multiplier exp) (caddr exp))
                (make-op (cons (make-product (multiplier exp) (caddr exp))
                                         (cdddr exp)))
              )
          )
    )
)

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (car p))

(define (multiplicand p)
    (if (null? (cdddr p)) (caddr p)
            (make-op (cddr p))
        )
)

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (base n) (cadr n))

(define (exponent n) (caddr n))

(define (exponentiation? n) (and (pair? n) (eq? (car n) '**)))

(define (make-exponentiation b e)
    (cond ((= e 0) 1)
          ((= e 1) b)
          (else (list '** b e))
        )
)

; the answer for this is (1 + (3 * (1 + y))) - is not 100% conveniet but could be simplified
(define a '(x + 3 * (x + y * x + 2)))
(define b '(x + 3 * x + 4))

(augend a)

(deriv a 'x)
(deriv b 'x)