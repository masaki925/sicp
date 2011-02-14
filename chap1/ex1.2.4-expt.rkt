;(define (print x) (display x) (newline))
(define count 0)

; ex1.2.4
;-- recursive
(define (expt-rec b n)
  (if (= n 0)
      1
      (* b (expt-rec b (- n 1)))))

;(trace expt-rec)
;(expt-rec 2 6)

;>(expt-rec 2 6)
;> (expt-rec 2 5)
;> >(expt-rec 2 4)
;> > (expt-rec 2 3)
;> > >(expt-rec 2 2)
;> > > (expt-rec 2 1)
;> > > >(expt-rec 2 0)
;< < < <1
;< < < 2
;< < <4
;< < 8
;< <16
;< 32
;<64
;64


;-- repetitive
(define (expt-rep b n)
  (expt-iter b n 1))

(define (expt-iter b counter product)
  (if (= counter 0)
      product
      (expt-iter b
                 (- counter 1)
                 (* b product))))

;(trace expt-rep)
;(trace expt-iter)
;(expt-rep 2 6)

;>(expt-rep 2 6)
;>(expt-iter 2 6 1)
;>(expt-iter 2 5 2)
;>(expt-iter 2 4 4)
;>(expt-iter 2 3 8)
;>(expt-iter 2 2 16)
;>(expt-iter 2 1 32)
;>(expt-iter 2 0 64)
;<64
;64

;-- fast
(define (square x) (* x x))

(define (fast-expt b n)
  (set! count 0)
  (display (fast-expt-do b n))
  (newline)
  (display "count: ")
  (display count))

(define (fast-expt-do b n)
  (set! count (+ count 1))
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt-do b (/ n 2))))
        (else (* b (fast-expt-do b (- n 1))))))

(trace fast-expt-do)
(fast-expt 2 6)
;(fast-expt 2 12)
;(fast-expt 2 24)
;(fast-expt 2 24)
;(fast-expt 2 24)

;>(fast-expt-do 2 6)
;> (fast-expt-do 2 3)    # (square (fast-expt b (/ n 2)))
;> >(fast-expt-do 2 2)
;> > (fast-expt-do 2 1)
;> > >(fast-expt-do 2 0)
;< < <1
;< < 2
;< <4
;< 8
;<64                  # (square (fast-expt b (/ n 2)))
;64
