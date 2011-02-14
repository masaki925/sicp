(require "../common.scm")

;* 2.2 階層データ構造と閉包性
;- cons の閉包性 => 要素が対であるような対を作る能力
;         ^^^^^^その演算を使って何かを組み合わせた結果がまた同じ演算を使って組み合わせられる
;- 階層的構造 => 部品からできた構造で、その部品がまたその部品からできている

;* 2.2.1 並びの表現
;- list は入れ子のcons で作られた対の並び
;-- 鎖の最後の対のcdr、対でない特別な値、並びの終わりを示す : nil

(define one-through-four (list 1 2 3 4))
(mydisplay one-through-four)

(display "car: ")
(mydisplay (car one-through-four))
(display "cdr: ")
(mydisplay (cdr one-through-four))

;------------
;リスト演算: 2つの慣習的プログラミング
(mydisplay  "1) cdr ダウン")

;リストからn番目の要素を抽出
(define (list-ref items n)
  (if (= n 0)
    (car items)
    (list-ref (cdr items) (- n 1))))

(define squares (list 1 4 9 16 25))

(mydisplay "\n(list-ref square 3): ")
(trace list-ref)
(mydisplay (list-ref squares 3))


;リストの長さを求める
(define odds (list 1 3 5 7))

(mydisplay "\n* 再帰的")
(define (mylength items)
  (if (null? items)
    0
    (+ 1 (mylength (cdr items)))))

(mydisplay "(mylength (list 1 3 5 7)): ")
(trace mylength)
(mydisplay (mylength odds))

(mydisplay "\n* 反復的")
(define (mylength2 items)
  (define (length-iter a count)
    (if (null? a)
      count
      (length-iter (cdr a) (+ 1 count))))
  (trace length-iter)
  (length-iter items 0))

(mydisplay "(mylength (list 1 3 5 7)): ")
(mydisplay (mylength2 odds))

;---------------------
(mydisplay "------------")
(mydisplay  "2) cons アップ")
(define (myappend list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (myappend (cdr list1) list2))))

(mydisplay "(myappend squares odds): ")
(trace myappend)
(mydisplay (myappend squares odds))

