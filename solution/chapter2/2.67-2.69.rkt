#lang sicp

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
(list left
right
(append (symbols left) (symbols right))
(+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))

(define (symbols tree)
(if (leaf? tree)
(list (symbol-leaf tree))
(caddr tree)))

(define (weight tree)
(if (leaf? tree)
(weight-leaf tree)
(cadddr tree)))

(define (decode bits tree)
(define (decode-1 bits current-branch)
(if (null? bits)
'()
(let ((next-branch
(choose-branch (car bits) current-branch)))
(if (leaf? next-branch)
(cons (symbol-leaf next-branch)
(decode-1 (cdr bits) tree))
(decode-1 (cdr bits) next-branch)))))
(decode-1 bits tree))

(define (choose-branch bit branch)
(cond ((= bit 0) (left-branch branch))
((= bit 1) (right-branch branch))
(else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (adjoin-set x set)
(cond ((null? set) (list x))
((< (weight x) (weight (car set))) (cons x set))
(else (cons (car set)
(adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
(if (null? pairs)
'()
(let ((pair (car pairs)))
(adjoin-set (make-leaf (car pair) ; symbol
(cadr pair)) ; frequency
(make-leaf-set (cdr pairs))))))

(define sample-tree
(make-code-tree (make-leaf 'A 4)
(make-code-tree
(make-leaf 'B 2)
(make-code-tree
(make-leaf 'D 1)
(make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;2.68
(define (element-of-set? x set)
(cond ((null? set) false)
((eq? x (car set)) true)
(else (element-of-set? x (cdr set)))))

;2.68
(define (correct-branch? symb branch)
    (element-of-set? symb (symbols branch))
)

;2.68
;redo
(define (encode-symbol symb tree)
    (if (leaf? tree) '()
        (cond ((correct-branch? symb (left-branch tree))  (cons '0 (encode-symbol symb (left-branch tree))))
              ((correct-branch? symb (right-branch tree)) (cons '1 (encode-symbol symb (right-branch tree))))
              (else (error "cannot find symbol  = " symb))
        )
    )
)

(define (encode message tree)
(if (null? message)
'()
(append (encode-symbol (car message) tree)
(encode (cdr message) tree))))

(define msg (decode sample-message sample-tree))

(encode msg sample-tree)

;2.69
(define (generate-huffman-tree pairs)
(successive-merge (make-leaf-set pairs)))

;2.69
;redo
(define (successive-merge leaf-set)
    (cond ((null? (cdr leaf-set)) (car leaf-set))
          (else (successive-merge (adjoin-set (make-code-tree (car leaf-set) (cadr leaf-set)) (cddr leaf-set))))
    )
)


(generate-huffman-tree '((A 4) (B 2) (C 1) (D 1)))

;2.70
(generate-huffman-tree '((A 2) (BOOM 1) (GET 2) (JOB 2) (SHA 3) (NA 16) (WAH 1) (YIP 9)))