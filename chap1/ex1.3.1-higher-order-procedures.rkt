(define (sum-integers a b)
  (if (> a b)
      0
      (+ a (sum-integers (+ a 1) b))))

(sum-integers 2 4)

;##################################
(define (cube x) (* x x x))

(define (sum-cubes a b)
  (if (> a b)
      0
      (+ (cube a) (sum-integers (+ a 1) b))))

(sum-cubes 2 4)

;##################################
(define (pi-sum a b)
  (if (> a b)
      0
      (+ (/ 1.0 (* a (+ a 2))) (pi-sum (+ a 4) b))))

(pi-sum 2 4)

;##################################

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

(define (inc n) (+ n 1))

(define (sum-cubes2 a b)
  (sum cube a inc b))

(sum-cubes2 1 10)

;##################################

(define (identity x) x)

(define (sum-integers a b)
  (sum identity a inc b))

(sum-integers 1 10)

;##################################

(define (pi-sum2 a b)
  (define (pi-term x)
    (/ 1.0 (* x (+ x 2))))
  (define (pi-next x)
    (+ x 4))
  (sum pi-term a pi-next b))

(* 8 (pi-sum2 1 1000))
