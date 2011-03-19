(require "../common.scm")

(p "問題2.27: deep-reverse")

(define x (list (list 1 2) (list 3 4)))
(define y (list (list 1 2) (list 3 (list 4 5))))

(define (deep-reverse l)
  (if (pair? l)
    (reverse (map deep-reverse l))
    l))

(p (deep-reverse y))
