(require "../common.scm")

(p "ex2.17: 与えられた(空でない) リストの最後の要素だけからなるリストを返す手続き last-pair")

(define odds (list 1 3 5 7))

;(define (last-pair items)
;  (if (= (length items) 1)
;    items
;    (last-pair (cdr items))))

(define (last-pair items)
  (let ((rest (cdr items)))
    (if (null? rest)
      items
      (last-pair rest))))

(trace last-pair)
(p "(last-pair (list 1 3 5 7))\n")
(p (last-pair odds))
;(display (last-pair '()))

