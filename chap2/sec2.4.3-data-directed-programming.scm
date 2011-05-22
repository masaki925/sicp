;(require "../common.scm")
(require "./table.scm")

; p.105 2.4.3 データ主導プログラミングと加法性

; 型による振り分け
;   2つの弱点
;      i. 汎用インタフェース手続きは、異なる表現のすべてを知らなければいけない
;     ii. システム全体でどの2つの手続きも同じ名前を持たないことを保証しなければならない
;   => 加法的でない ;      => データ主導プログラミングによって、システム設計をさらに部品化することで解決 ;         => 演算名、型名、実際の手続きの対応を表にまとめる

(define (attach-tag type-tag contents)
    (cons type-tag contents))

(define (type-tag datum)
    (if (pair? datum)
          (car datum)
              (error "Bad tagged datum -- TYPE-TAG" datum)))
(define (contents datum)
    (if (pair? datum)
          (cdr datum)
              (error "Bad tagged datum -- CONTENTS" datum)))

(define (install-rectangular-package)
  ;; 内部手続き
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; システムの他の部分とのインタフェース
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

; polar
(define (install-polar-package)
  ;; 内部手続き
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

  ;; システムの他の部分とのインタフェース
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error
          "No method for these types -- APPLY-GENERIC"
          (list op type-tags))))))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

;----

(install-rectangular-package)
(install-polar-package)
(define z1 (make-from-real-imag 1 2))
(define z2 (make-from-mag-ang 2 30))

(p "* rectangular")
(p (real-part z1))
(p (imag-part z1))
(p (magnitude z1))
(p (angle z1))


(p "\n* polar")
(p (real-part z2))
(p (imag-part z2))
(p (magnitude z2))
(p (angle z2))


(p "-----------------------\n")

(p "* ex.2.73\n")
; p.108
; ex.2.73

;
; * 2.3.2節の微分 => p.86 begin -----------------------------
;
(define (deriv-pre exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv-pre (addend exp) var)
                   (deriv-pre (augend exp) var)))
        ((product? exp)
         (make-sum
           (make-product (multiplier exp)
                         (deriv-pre (multiplicand exp) var))
           (make-product (deriv-pre (multiplier exp) var)
                         (multiplicand exp))))
        ((exponentiation? exp)
         (make-product (exponent exp)
                        (make-product (make-exponent (base exp)
                                                     (- (exponent exp) 1))
                                      (deriv-pre (base exp) var))))
        ; other
        (else (error "unknown expression type -- DERIV" exp))))

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
;
; * 2.3.2節の微分 => p.86 end   -----------------------------
;

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; a-1. 上でやったことを説明せよ
;      - number? とvariable? 以外の演算子を'型' として表に登録し、対応する手続きを表から選択するようにした。# データ主導型プログラミング
;        これによって、deriv を編集することなく、演算子の追加が行えるようになった。
; a-2. number? やvariable? がデータ主導の振り分けに吸収できないのはなぜか
;      - '+ や'* とは違って、大量にエントリーを追加しなくてはいけなくなるから。 '0, '1, ...
;
; b. 和と積の微分の手続きを書き、上のプログラムで使う表に、それらを設定するのに必要な補助プログラムを書け

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
(trace deriv-pre)
(trace get)
(trace make-exponent)

#| ; comment begin ===========================
(p "\n** deriv e1 'x")
(p (deriv e1 'x))
(p (deriv-pre e1 'x))
(p "\n** deriv-pre e2 'x")
(p (deriv-pre e2 'x))

(p "\n** deriv-pre e3 'x")
(p (deriv-pre e3 'x))

(p "\n** deriv e2 'x")
(p (deriv e2 'x))
|# ; comment end   ===========================

(p "\n** deriv-pre e3 'x")
(p (deriv-pre e3 'x))

(p "\n** deriv e3 'x")
(p (deriv e3 'x))

;----
; package の中でaddend, augend を使ってしまったときのエラー
;deriv e1 'x
;CALL deriv (+ x x) x
; CALL deriv x x
; RETN deriv 1
;gosh: "error": pair required, but got ()

