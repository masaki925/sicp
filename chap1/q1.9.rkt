(define (inc x) (+ x 1))
(define (dec x) (- x 1))

(define (myplus1 a b)
  (trace myplus1)
  (if (= a 0)
      b
      (inc (myplus1 (dec a) b))))

(define (myplus2 a b)
  (trace myplus2)
  (if (= a 0)
      b
      (myplus2 (dec a) (inc b))))
