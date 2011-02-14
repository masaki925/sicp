;p30 q1.25

(define (square x) (* x x))

; p.28 expmod in fermat-test #########
; base^exp MOD m
(define (expmod-f base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod-f base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod-f base (- exp 1) m))
                    m))))

; Alyssa ##############
;p.25
(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

(define (expmod base exp m)
  (remainder (fast-expt base exp) m))
;######################

;(trace expmod-f)
(expmod-f 400000 250000 8)
(expmod-f 400000 250000 8)

;(trace expmod)
;(trace fast-expt)
(expmod 400000 250000 8)
(expmod 400000 250000 8)


;>(expmod-f 4 5 5)
;> (expmod-f 4 4 5)
;> >(expmod-f 4 2 5)
;> > (expmod-f 4 1 5)
;> > >(expmod-f 4 0 5)
;< < <1
;< < 4
;< <1
;< 1
;<4
;4
;>(expmod 4 5 5)
;> (fast-expt 4 5)
;> >(fast-expt 4 4)
;> > (fast-expt 4 2)
;> > >(fast-expt 4 1)
;> > > (fast-expt 4 0)
;< < < 1
;< < <4
;< < 16
;< <256
;< 1024
;<4
;4
