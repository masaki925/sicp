(require "../common.scm")

(p "問題2.23: for-each")

(p "作用だけさせたい。let, begin, cond のelse 節で作用させる、などの方法がある")
(define (for-each-1 f l)
  (cond ((null? l) (newline))
    (else)
