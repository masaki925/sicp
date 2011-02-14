; a1 = bq + aq + ap
; b1 = bp + aq

; a2 = b1q + a1q + a1p
;    = (bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p
;    = bpq + aqq + bqq + aqq + apq + bpq + apq + app
;    = bpq + bqq + bpq + aqq + aqq + apq + apq + app
;    = b(pq + qq + pq) + a(qq + qq + pq + pq + pp)
;    = b(2pq + qq) + a(qq + 2pq) + a(pp + qq)
;    
;    p' = pp + qq
;    q' = 2pq + qq


; b2 = b1p + a1q
;    = (bp + aq)p + (bq + aq + ap)q
;    = bpp + apq + bqq + aqq + apq
;    = bpp + bqq + apq + aqq + apq
;    = b(pp + qq) + a(pq + qq + pq)
;    = b(pp + qq) + a(2pq + qq)
;    
;    p' = pp + qq
;    q' = 2pq + qq

(define (square x) (* x x))

(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a
                   b
                   (+ (square p) (square q))
                   (+ (* 2 p q) (square q))
                   (/ count 2)))
        (else (fib-iter (+ (* b q) (* a q) (* a p))
                        (+ (* b p) (* a q))
                        p
                        q
                        (- count 1)))))

(trace fib-iter)
(fib 10)
