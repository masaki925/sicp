(require "../common.scm")

; p.160 ex3.27
; memorization (tabulation) は手続きに以前計算した値を局所表に記録させる技法である
; この技法はプログラムの効率に大きな差をつけることがある
; メモ化の手続きは表を維持し、そこに過去の呼び出しの値を、その値を生じた引数をキーとして格納する
; メモ化手続きが値を計算するよう頼まれると、まずその値がすでにそこに存在するか表を調べ、それがあればその値を返す
; それがなければ通常の方法で新しい値を計算し、これを表に格納する
; メモ化の例として、1.2.2節のFibonacci 数を計算する指数的プロセスを思い出そう

; ---------------
(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))
; ---------------


(define (fib n)
  (p "fib: ")
  (p n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

(define (memoize f)
  (let ((table (make-table)))
    (lambda (x)
      (let ((previously-computed-result (lookup x table)))
        (or previously-computed-result
            (let ((result (f x)))
              (insert! x result table)
;              (p table)
              result))))))

;(define memo-fib
;  (memoize (lambda (n)
;             (p "memo-fib:")
;             (p n)
;             (cond ((= n 0) 0)
;                   ((= n 1) 1)
;                   (else (+ (memo-fib (- n 1))
;                            (memo-fib (- n 2))))))))

; q1. (memo-fib 3) の計算を解析する環境の図を描け
; (let ((var exp)) body)
; = ((lambda (var) (body)) exp) # var のところにexp をいれたら何が起こるか

(trace memoize)
(trace fib)


; q2. memo-fib がn 番目のFibonacci 数をn に比例したステップ数で計算できる理由を説明せよ
;   => 同じ値を2度と計算しない。
;   ==> 但し、初回はO(n)。2回目以降はO(1) で計算可能。
;   ===> 但し、list を辿るのにO(n) かかるので、計算量はO(n)。

; q3. この方式はmemo-fib を単に(memoize fib) と定義してもやはり働くだろうか
; => 動きはするが、result を求めるときにfib を呼んでしまうから、表の参照が使えてない

; NOTE: dont forget comment out original memo-fib
(define memo-fib
  (memoize fib))

(trace memo-fib)

(memo-fib 40)

