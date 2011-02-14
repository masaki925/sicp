;p.31 q1.26

(require srfi/27)
(define random random-integer)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
;         (remainder (* (expmod base (/ exp 2) m)
;                       (expmod base (/ exp 2) m))
;                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

(trace expmod)
(expmod 2 3 3)
