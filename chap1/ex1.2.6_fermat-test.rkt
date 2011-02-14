(require srfi/27)
(define random random-integer)

(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))

;(expmod 2 3 8)

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
(trace try-it)
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) #t)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else #f)))

(trace fermat-test)
(trace fast-prime?)
(fast-prime? 23 5)
