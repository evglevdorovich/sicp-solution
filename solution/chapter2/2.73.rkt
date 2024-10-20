#lang racket

(require compatibility/mlist)

(define (make-table)
  (let ((local-table (mlist '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (mcdr record)
                  #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (massoc key-1 (mcdr local-table))))
        (if subtable
            (let ((record (massoc key-2 (mcdr subtable))))
              (if record
                  (set-mcdr! record value)
                  (set-mcdr! subtable
                            (mcons (mcons key-2 value)
                                  (mcdr subtable)))))
            (set-mcdr! local-table
                      (mcons (mlist key-1
                                  (mcons key-2 value))
                            (mcdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))


(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (variable? x) (symbol? x))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) (operands exp)
                                            var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (make-sum a1 a2)
    (cond ((=number? a1 0) a2)
          ((=number? a2 0) a1)
          ((and (number? a1) (number? a2)) (+ a1 a2))
          (else '(a1 + a2))
))

(define (make-product m1 m2)
    (cond ((or (=number? m1 0) (=number? m2 0)) 0)
          ((=number? m1 1) m2)
          ((=number? m2 1) m1)
          ((and (number? m1) (number? m2)) (* m1 m2))
          (else '(m1 * m2))
    )
)

(define (sum? x) (and (pair? x) (eq? (cadr x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x) (and (pair? x) (eq? (cadr x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (=number? exp num) (and (number? exp) (= exp num)))

(define (install-product-package)
;; internal procedures
(define (deriv-product exp var)
    (make-sum (make-product (multiplier exp)
                     (deriv (multiplicand exp) var))
                      (make-product (deriv (multiplier exp) var)
                                    (multiplicand exp)))
)
;; interface to the rest of the system
(put 'deriv '* deriv-product))

;2.73 d -> (put '* 'deriv deriv-product))

(define (install-sum-package)
;; internal procedures
(define (deriv-sum exp var)
    (make-sum (deriv (addend exp) var)
            (deriv (augend exp) var))
)
;; interface to the rest of the system
(put 'deriv '+ deriv-sum))

(define (install-eksp-package)

(define (base n) (car n))
(define (exponent n) (cadr n))

;; internal procedures
(define (deriv-eksp exp var)
    (make-product
                (make-product (exponent exp) (make-exponentiation (base exp) (- (exponent exp) 1)))
                (deriv (base exp))
            )
)
;; interface to the rest of the system
(put 'deriv '** deriv-eksp))

(install-sum-package)
(install-product-package)
