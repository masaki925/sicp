(require "../common.scm")

(mydisplay "ex2.20: 1つかそれを超える個数の整数をとり、先頭と同じ偶奇性を持つ引数のリストを返す手続き same-parity\n")

; - 偶数、奇数で場合分けしてfunc(even?/odd?) を渡す
;   - cdr ダウン
;   -- func がtrue のときのみcons
(define (same-parity first . rest)
  (define (build-list func items)
    (if (null? items)
      nil
      (if (func (car items))
        (cons (car items) (build-list func (cdr items)))
        (build-list func (cdr items)))))
  (trace build-list)
  (if (even? first)
    (cons first (build-list (lambda (x) (even? x)) rest))
    (cons first (build-list (lambda (x) (odd? x)) rest))))

(trace same-parity)
(mydisplay "\n* (same-parity 1 2 3 4 5 6 7)")
(mydisplay (same-parity 1 2 3 4 5 6 7))
;----
(mydisplay "\n* (same-parity 2 3 4 5 6 7)")
(mydisplay (same-parity 2 3 4 5 6 7))

