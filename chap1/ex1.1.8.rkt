(define true #t)
(define false #f)

(define (disp x)
  (display x)
  (newline))

;(define (square x)
;  (* x x))

(define (square x)
  (exp (double (log x))))

(define (double x) (+ x x))

(square 3)