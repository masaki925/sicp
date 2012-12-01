(require "../common.scm")
(require "./prime.scm")

; p.186 並列性、時 および通信

; - 共有状態はどこで決まるか？
; -- 異なるプロセスを同期させ、共有状態を確定させ、事象の順を強制させるためには、プロセス間の通信が必要。

; p.187 3.5 ストリーム

; - 状態をモデル化するのに代入ではなく、ストリームというモデル化を扱ってみる。
; - 代入の複雑さはどこから来たか？
; -- 実世界の時間変化を、計算機内の時間変化と同一視した。
; --- 同一視しなきゃダメか？
; ---- 全時間の歴史に注目すれば、それぞれの時間の変化は関係ない。「それx(t)」は「それx(t)」。
; - ストリームというデータ構造を使って、システムの時間史を表現する並びを使って、変化をモデル化してみる。
; -- 遅延評価という技法を使う

; p.188 3.5.1 ストリームは遅延リスト

; - map, filter, accumulate は優美だが、厳しい日効率の代価として得られている
; -- 標準的反復の例
(define (sum-primes-naive a b)
  (define (iter count accum)
    (cond ((> count b) accum)
          ((prime? count) (iter (+ count 1) (+ count accum)))
          (else (iter (+ count 1) accum))))
  (trace iter)
  (iter a 0))


; -- 並びの演算を使った例
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (enumarate-interval low high)
  (if (> low high)
    nil
    (cons low (enumarate-interval (+ low 1) high))))


(define (sum-primes a b)
  (accumulate +
              0
              (filter prime? (enumarate-interval a b))))







;; EXEC
;(p "\n * test overhead -----------------\n\n")
;(p (sum-primes-naive 10 20))
;(p (sum-primes 10 20))

;(p (car (cdr (filter prime? (enumarate-interval 100 1000)))))
;(p (car (cdr (filter prime? (enumarate-interval 1000 100000)))))
;(p (car (cdr (filter prime? (enumarate-interval 10000 100000)))))
;(p (car (cdr (filter prime? (enumarate-interval 10000 1000000)))))

;; すべてのリストを作成し、全てについて素数性のテストをかけて、最終的には結果のほとんどを無視する。非効率。
;
;----
; ストリームを部分的に構成するようにし、その部分構成を、ストリームが消費するプログラムへ渡す。
; 消費側がまだ構成されてないストリームへアクセスしようとすると、ストリームは要求された部分を作るべく、自分のもう少し十分な分だけを自動的に構成し、ストリーム全体が存在するかのような幻想を保つ。

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))





(define (stream-map proc s)
  (if (stream-null? s)
    the-empty-stream
    (cons-stream (proc (stream-car s))
                 (stream-map proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
    'done
    (begin (proc (stream-car s))
           (stream-for-each proc (stream-cdr s)))))

(define (display-line x)
  (newline)
  (display x))

(define (display-stream s)
  (stream-for-each display-line s))

;---
; - ストリームと通常のリストの関係。
; -- 有理数の評価の話。分数を既約とするのを構成時とするか選択時とするかの違い。
; -- ストリームではcdr の選択時に評価される
; 
; delay とforce の実装

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin 
               (set! result (proc))
               (set! already-run? true)
               result)
        result))))

(define-macro (delay x)
              `(memo-proc (lambda () ,x)))

(define (force delayed-object) (delayed-object))

;(define-macro (delay x)
;              `(lambda () ,x))


(define-macro (cons-stream a b)
              `(cons ,a (delay ,b)))

; ---
(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())
(define (stream-null? x) (null? x))

(define (stream-enumarate-interval low high)
  (if (> low high)
    the-empty-stream
    (cons-stream
      low
      (stream-enumarate-interval (+ low 1) high))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter pred
                                     (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

; EXEC
;(p "\n * p.190 - 191 test stream version has less overhead -----------------\n\n")
;;(trace stream-filter)
;;(trace stream-car)
;;(trace stream-cdr)
;(p (stream-car
;     (stream-cdr
;       (stream-filter prime?
;                      (stream-enumarate-interval 10000 1000000)))))
;                      ;(stream-enumarate-interval 100 1000)))))

; -------
;(define x (stream-filter prime?
;                         (stream-enumarate-interval 10 100)))
; EXEC
;(p "\n * confirm stream car -----------------\n\n")
;(p (stream-car x))


; -----
; p.192 問題3.50
; Q. stream-map を完成させよ
;
; (map + (list 1 2 3) (list 40 50 60))
; #=> (41 52 63)
;
; p.60 map-naive

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map
            (cons proc (map stream-cdr argstreams))))))

;(trace stream-map)

(define s1 (stream-enumarate-interval 1 10))
(define s2 (stream-enumarate-interval 11 100))
(define s3 (stream-enumarate-interval 101 1000))
(define strm (stream-map + s1 s2 s3))

; EXEC
;(p (stream-ref strm 0)) ; 1 + 11 + 101 = 113
;(p (stream-ref strm 1)) ; 2 + 12 + 102 = 116
;(p (stream-ref strm 2)) ; 3 + 13 + 103 = 119

;----
; p.192 問題3.51
; Q. 解釈系は、何を印字するか？
; A. memo 化されてる。評価されるときに表示される

;        CALL stream-ref (4 . #[proc]) 1
;          CALL memo-proc #[proc]
;          RETN memo-proc #[proc]
;          CALL stream-map #[proc] (5 . #[proc])
;            CALL display-line 5

; EXEC
(define (show x)
  (display-line x)
  x)


;(trace stream-map)
(define x (stream-map show (stream-enumarate-interval 0 10))) ; (0 1 2 ...)
;(define y (stream-map show (stream-enumarate-interval 0 10))) ; (0 1 2 ...)
;=> (0 (delay ...))
(p x)

;(trace display-line)
;(trace stream-map)
;(trace stream-ref)
;(trace memo-proc)

;(stream-ref x 5)
;(p "---")
;(stream-ref x 7)
;(stream-ref x 11) ; error













;----
; p.192 問題3.52
; - (stream-filter pred) はstream-car した結果をpred してtrue だったら(cons (pred stream-car strem) (filter pred (stream-cdr stream)))
; - even? と remainder x 5 は、sum に対してかかってくる
;
; Q1. 上の式が評価し終わったとき、sum の値は何か
; A1. 210
;
; Q2. stream-ref とdisplay-stream 式の評価に応じて印字される応答は何か
; A2. 7番目にeven となるsum と、remainder 5 となるsum のすべて
;
; Q3. この応答は、delay をmem-proc の最適化を使わなかった時とどう違うか
; A3. accum がメモされないので、sum の足し込みが冗長に行われて結果が異なる

;; EXEC
;(trace stream-map)
;(trace stream-ref)
;(trace stream-filter)
;
(define sum 0)
(define (accum x)
  (set! sum (+ x sum))
  sum)
;(trace accum)
;
;(p "\n*** checkpoint 1 -------")
;(p "\n* sum") (p sum)
;
;(p "\n* (define seq (stream-map accum (stream-enumarate-interval 1 20)))")
(define seq (stream-map accum (stream-enumarate-interval 1 20)))
;(p "\n*** checkpoint 2 -------")
;(p "\n* sum") (p sum)
;(p "\n* seq") (p seq)
;;
;(p "\n* (define y (stream-filter even? seq))")
(define y (stream-filter even? seq))
;(p "\n*** checkpoint 3 -------")
;(p "\n* sum ") (p sum)
;(p "\n* seq") (p seq)
;(p "\n* y") (p y)
;
;(p "\n* (define z (stream-filter (lambda (x) (= (remainder x 5) 0)) seq))")
(define z (stream-filter (lambda (x) (= (remainder x 5) 0))
                         seq))
;(p "\n*** checkpoint 4 -------")
;(p "\n* sum") (p sum)
;(p "\n* seq") (p seq)
;(p "\n* y") (p y)
;(p "\n* z") (p z)
;
(stream-ref y 7)
;(p (stream-ref y 7))
;(p "\n*  (stream-ref y 7)")
;(p (stream-ref y 7))
;(p "\n*** checkpoint 5 -------")
;(p "\n* sum") (p sum)
;(p "\n* seq") (p seq)
;(p "\n* y") (p y)
;(p "\n* z") (p z)
;; 6, 10, 28, 36, 66, 120, 136
;
;(p "\n* (display-stream z)")
(display-stream z)
;(p "\n*** checkpoint 6 -------")
;(p "\n* sum") (p sum)
;(p "\n* seq") (p seq)
;(p "\n* y") (p y)
;(p "\n* z") (p z)













; example
;(p "hogehoge")
;(define (hoge x)
;  (p x)
;  x)
;
;(p (hoge 1))
;;(define promise (delay (hoge 2)))
;(define promise (memo-proc (lambda () (hoge 2))
;                 ))
;
;(p (force promise))
;(p (force promise))
;
;(define hoge2 (cons-stream 1 (hoge 3)))
;(define hoge2 (cons 1 (delay (hoge 3))))
;(define hoge2 (cons 1 (memo-proc (lambda() (hoge 3)))))
;
;(p (force hoge2))


