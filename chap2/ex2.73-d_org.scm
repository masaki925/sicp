;(require "../common.scm")
(require "./table.scm")

(p "* ex.2.73\n")
; p.108
; ex.2.73

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2) (+ a1 a2)))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))
(define (addend s) (cadr s))
(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))
(define (multiplier p) (cadr p))
(define (multiplicand p) (caddr p))


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

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

(define (install-op-package)
  ;; 内部手続き                
  (define (deriv-sum exp var)          ; 以前と比べると、演算子がない(operands exp) ので下記注意。
    (make-sum (deriv (car exp) var)    ;   addend(cadr) ではない
              (deriv (cadr exp) var))) ;   augend(caddr) ではない
  (define (deriv-product exp var)
    (make-sum
           (make-product (car exp)
                         (deriv (cadr exp) var))
           (make-product (deriv (car exp) var)
                         (cadr exp))))
  (define (deriv-exponent exp var)
    (make-product (cadr exp)
                  (make-product (make-exponent (car exp)
                                               (- (cadr exp) 1))
                                (deriv (car exp) var))))
 
  ;; インタフェース
  (put 'deriv '+ deriv-sum)
  (put 'deriv '* deriv-product)
  (put 'deriv '** deriv-exponent)
  'done)

(install-op-package)

(define e1 (make-sum 1 'x))      (p (format "e1: ~s" e1))
(define e2 (make-product 2 'x))  (p (format "e2: ~s" e2))
(define e3 (make-exponent 'x 3)) (p (format "e3: ~s" e3))

(trace deriv)
(trace get)
(trace make-exponent)

#| ; comment begin ===========================
(p "\n** deriv e1 'x")
(p (deriv e1 'x))

(p "\n** deriv e2 'x")
(p (deriv e2 'x))
|# ; comment end   ===========================

(p "\n** deriv e3 'x")
(p (deriv e3 'x))

