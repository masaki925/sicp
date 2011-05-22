(require "../common.scm")

; p.99 2.4 抽象データの多重表現
; p.46 の有理数の実装、抽象の壁によって、"有理数の使い方"を"データ表現"から隔離した。
; 複雑さを制御する強力な道具だが、データ抽象はまだ十分に強力ではない。
; 例えばデータオブジェクトの表現が複数ある場合

; 複素数は2つの等価な方法、直交座標形式、極座標形式 にて表現できる。
; 複素数にはそれぞれ異なる演算に適した2つの異なる表現がある。

; 複数の表現を扱うシステムの設計には、異なる設計選択を相互に隔離し、1つのプログラムの中に異なる選択が共存できるようにする抽象の壁が必要。
; => 汎用手続きを使う。
;    => 型タグを持つデータオブジェクト
;    => データ主導プログラミング

; 複素数の例
; 水平の壁と、垂直な壁ができる


; 実部と虚部で指定した複素数を返す
; z = x + yi
(make-from-real-imag (real-part z) (imag-part z))

; 絶対値と偏角で指定した複素数を返す
; z = r (cosA + i sinA)
(make-from-mag-ang (magnitude z) (angle z))


(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))


; 三角法
; x = r cos A , r = route(x^2 + y^2)
; y = r sin A , A = arctan(y, x) # 逆正接関数。atan 手続き。 y/x になる角度を返す

; Ben: 直交座標形式 begin ----------------------
(define (real-part z) (car z))
(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z)) (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a)) (* r (sin a))))
; Ben: 直交座標形式 end ----------------------

; Alyssa: 極座標形式 begin ------------------
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y))) ; r = x^2 + y^2
        (atan y x)))                     ; A = arctan(y, x)

(define (make-from-mag-ang r a) (cons r a))
; Alyssa: 極座標形式 end ---------------------


