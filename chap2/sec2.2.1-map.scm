(require "../common.scm")

(mydisplay "■ p.59 リストの写像 ----------------")

(mydisplay "* リストの各数値を与えられた係数倍する")
(define (scale-list items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))

(trace scale-list)
(mydisplay (scale-list (list 1 2 3 4) 10))

;-------------------------
; map
(define (map proc items)
  (if (null? items)
    nil
    (cons (proc (car items))
          (map proc (cdr items)))))

(trace map)
(mydisplay "\n* (map abs (list -10 2.5 -11.6 17))\n")
(map abs (list -10 2.5 -11.6 17))

(mydisplay "\n* (map (lambda (x) (* x x))\n       (list 1 2 3 4))\n")
(map (lambda (x) (* x x)) (list 1 2 3 4))

;----
(define (scale-list2 items factor)
  (map (lambda (x) (* x factor))
       items))

(trace scale-list2)
(mydisplay "\n* (scale-list2 (list 1 2 3 4) 10)")
(scale-list2 (list 1 2 3 4) 10)

; map の重要性
; - 共通パターンの取り込み
; - より高い抽象化の達成
;   - 計算機が異なるプロセスを実行することではなく、
;     我々がプロセスを異なって考えること => 抽象の壁

; 比較用
;(mydisplay "* リストの各数値を与えられた係数倍する")
;
;(define (scale-list items factor)
;  (if (null? items)
;    nil
;    (cons (* (car items) factor)
;          (scale-list (cdr items) factor))))

