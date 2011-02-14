(require srfi/19)
(define runtime current-time)

; prime? ###########################
(define (square n) (* n n))

(define (smallest-divisor n)
  (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))
;#################################

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (time-difference (runtime) start-time))
      #f))

(define (report-prime elapsed-time)
  (display " *** ")
  (display (time-nanosecond elapsed-time)))

(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (search-for-primes a n)
  (search-for-primes-helper (next-odd a) 0 n))

(define (search-for-primes-helper a found n)
  (if (= found n)
      0
      (search-for-primes-helper (+ a 2)
                                (if (timed-prime-test a)
                                    (+ found 1)
                                    found)
                                n)))

(define (next-odd n)
  (if (even? n)
      (+ n 1)
      n))

(search-for-primes 10000000 3)
(search-for-primes 100000000 3)
