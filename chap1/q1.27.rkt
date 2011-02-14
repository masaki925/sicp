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

(define (fermat-test n a)
    (= (expmod a n n) a))

(define (complete-fermat-test-helper n a)
  (cond ((= a 0) #t)
        ((fermat-test n a) (fermat-test n (- a 1)))
        (else #f)))

(define (complete-fermat-test n)
  (display n)
  (complete-fermat-test-helper n (- n 1)))
 

(complete-fermat-test 1)
(complete-fermat-test 2)
(complete-fermat-test 3)
(complete-fermat-test 4)
(complete-fermat-test 5)
(complete-fermat-test 6)
(complete-fermat-test 7)
(complete-fermat-test 109)  ; prime
(complete-fermat-test 111)  ; can be divided by 3 
(complete-fermat-test 561)  ;                   3
(complete-fermat-test 1105) ;                   5
(complete-fermat-test 1729) ;                   7
(complete-fermat-test 2465) ;                   5
(complete-fermat-test 2821) ;                   7
(complete-fermat-test 6601) ;                   7

