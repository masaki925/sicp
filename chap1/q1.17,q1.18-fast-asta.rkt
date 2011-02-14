; recursive
(define (asta base times)
  (if (= times 0)
      0
      (+ base (asta base (- times 1)))))

;(trace asta)
;(asta 2 6)

;-- repetitive
(define (my-asta base times)
  (my-asta-iter base times 0))

(define (my-asta-iter base count product)
  (if (= count 0)
      product
      (my-asta-iter base (- count 1) (+ product base))))

;(trace my-asta-iter)
;(my-asta 2 6)


;-- fast-asta --------------------------
(define (double x) (+ x x))
(define (halve x) (/ x 2))

;--q1.17 recursive
(define (fast-asta base times)
  (cond ((= times 0) 0)
        ((even? times) (fast-asta (double base) (halve times)))
        (else (+ base (fast-asta base (- times 1))))))

;(trace fast-asta)
;(fast-asta 2 15)





;--q1.18 repetitive
(define (my-fast-asta base times)
  (fast-asta-iter base times 0))

(define (fast-asta-iter base times a)
  (cond ((= times 0) a)
        ((even? times) (fast-asta-iter (double base) (halve times) a))
        (else (fast-asta-iter base (- times 1) (+ a base)))))

;(trace fast-asta-iter)
;(my-fast-asta 2 15)
