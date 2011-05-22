(require "../common.scm")

(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))
(define (make-exponent m1 m2)
  (cond ((not (number? m2)) (error "exponent should be a number -- MAKE-EXPONENT" m1 m2))
        ((=number? m2 0) 1)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (expt m1 m2))
        (else (list '** m1 m2))))

(define e1 (make-exponent 2 3))

;(p (exponentiation? (list '** 2 2)))
(p (exponentiation? e1))

