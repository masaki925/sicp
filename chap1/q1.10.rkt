(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))

(trace A)
(define (f n) (A 0 n))

(f 0)

; 2乗
(expt 2 2)
