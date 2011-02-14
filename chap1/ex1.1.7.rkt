(define true #t)
(define false #f)

(define (disp x)
  (display x)
  (newline))

(define (square x)
  (* x x))

;(define (sqrt x)
;  (the y (and (>= y 0)
;              (= (square y) x))))

(define (sqrt-iter guess x)
  (disp guess)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))
