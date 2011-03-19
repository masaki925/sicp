(require "../common.scm")

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))









(p "問題2.34: Horner の方法を使って、xの多項式に対してあるx の値での評価値を計算する。")

(p "
n=0:
a0

n=1:
a1x1 + a0

n=2:
a2x^2 + a1x^1 + a0
  (a2x + a1)x + a0

n=3:
a3x3 + a2x2 + a1x1 + a0
  ((a3x + a2)x + a1)x + a0
…
")

; coefficient: 係数
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

; y = 1 + 3x + 5x^3 + x^5
;   = 1 + 3x + 0x^2 + 5x^3 + 0x^4 + x^5
;   = 1x^5 + 0x^4 + 5x^3 + 0x^2 + 3x + 1
; 
; * 手動
; n=5, x=2:
; 1*2^5 + 5*2^3 * 3*2 * 1
; = 32 + 40 + 6 + 1
; = 79
;
; * Horner's rule
; 1*2 + 0
; (1*2 + 0) * 2 + 5
; ((1*2 + 0) * 2 + 5) * 2 + 0
; (((1*2 + 0) * 2 + 5) * 2 + 0) * 2 + 3
;
; ((((1*2 + 0) * 2 + 5) * 2 + 0) * 2 + 3) * 2 + 1
;       2      * 2 + 5
;   (   9             ) * 2 + 0
;  (    18                     ) * 2 + 3
; (     39                              ) * 2 + 1 = 79

(trace accumulate)
(trace horner-eval)
(p "(horner-eval 2 (list 1 3 0 5 0 1))")
(p (horner-eval 2 (list 1 3 0 5 0 1)))

