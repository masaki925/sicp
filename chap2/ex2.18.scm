(require "../common.scm")

(mydisplay "ex2.18: 引数としてリストをとり、同じ要素の逆順のリストを返す手続き reverse")

(define odds (list 1 3 5 7))

(mydisplay "1) cons アップしたもの")
(define (myreverse items)
(let ((rest (cdr items)))
  (if (null? rest)
    (car items)
    (cons (myreverse (cdr items)) (cons (car items) nil)))))

(mydisplay "(myreverse (list 1 3 5 7))\n")
(trace myreverse)
(mydisplay (myreverse odds))

(mydisplay "\n=> list の最後を意味するnil を入れてしまうと、list の入れ子になってしまう。\n")

;------------------------------


(mydisplay "----------------------")
(mydisplay "2) append したもの")
(define (myreverse2 l)
  (define (myreverse2-iter items r)
    (if (null? items)
        r
        (myreverse2-iter (cdr items) (cons (car items) r))))
  (trace myreverse2-iter)
  (myreverse2-iter l '()))

(mydisplay "(myreverse2 (list 1 3 5 7))\n")
(trace myreverse2)
(mydisplay (myreverse2 odds))
;(mydisplay (myreverse2 '()))

;- 並びの最後はnil で表現する
;- cons でリストに値を追加するには (cons (値) (list))

