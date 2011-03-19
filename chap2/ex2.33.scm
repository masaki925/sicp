(require "../common.scm")

(p "問題2.33: リスト操作の基本演算の、アキュムレーションとしての定義")

(p "sequence を渡して、それぞれに対してop を作用させていく。初期値はinitial ")

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(p "* map")
; 参考: p.59 scale-list: items の各要素をfactor倍した結果をリストにして返す
(define (scale-list items factor)
  (if (null? items)
    nil
    (cons (* (car items) factor)
          (scale-list (cdr items) factor))))
;(scale-list (list 1 2 3 4 5) 10)
;#=> (10 20 30 40 50)

; items とsequence が同じことなので、"factor倍した結果をリストにする" を実装する
; y がsequence の最後のaccumulate されたsequence. (cons (pr x) y) は、1つ前にaccumulate された結果(y) にたいして、(pr x) がcons に作用される。
(define (mymap pr sequence)
  (accumulate (lambda (x y) (cons (pr x) y))
              nil
              sequence))

(trace accumulate)
(trace mymap)
(p (mymap (lambda (x) (* x x)) (list 1 2 3)))


(p "* append")
; 参考: p.58 append
(define (append-pre list1 list2)
  (if (null? list1)
    list2
    (cons (car list1) (append-pre (cdr list1) list2))))
; (append-pre (list 1 4 9 16) (list 1 3 5 7))
; #=> (1 4 9 16 1 3 5 7)

(define (myappend seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(trace accumulate)
(trace myappend)
(p (myappend (list 1 2) (list 3 4)))

; cdr ダウンしていって、initial にあたったところで、car アップしていく
;CALL myappend (1 2) (3 4)
; CALL accumulate #[proc] (3 4) (1 2)
;  CALL accumulate #[proc] (3 4) (2)
;   CALL accumulate #[proc] (3 4) ()
;   RETN accumulate (3 4)
;  RETN accumulate (2 3 4)
; RETN accumulate (1 2 3 4)
;RETN myappend (1 2 3 4)
;(1 2 3 4)


(p "* length")
; 参考: p.57 length
(define (length-pre items)
  (if (null? items)
    0
    (+ 1 (length-pre (cdr items)))))
; (length (list 1 3 5 7))
; #=> 4

; 1つ前の結果(y) は、これまでの加算結果。それにどんどん足していく。
(define (mylength sequence)
  (accumulate (lambda (x y) (+ 1 y))
              0
              sequence))

(trace accumulate)
(trace mylength)
(p (mylength (list 1 2 3 4)))

