(require "../common.scm")

(p "ex2.21: 引数として数のリストをとり、これらの数の2乗のリストを返す square-list")

(define (square-list items)
  (if (null? items)
    nil
    (cons (* (car items) (car items))
          (square-list (cdr items)))))

(p "\n* (square-list (list 1 2 3 4))")
(trace square-list)
(p (square-list (list 1 2 3 4)))

(define (square-list2 items)
  (map (lambda (x) (* x x)) items))

(p "\n* (square-list2 (list 1 2 3 4))")
(trace square-list2)
(trace map)
(p (square-list2 (list 1 2 3 4)))

