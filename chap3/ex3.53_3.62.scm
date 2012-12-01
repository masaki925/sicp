(require "../common.scm")

(define the-empty-stream '())
(define (stream-null? x) (null? x))

(define (stream-ref s n)
  (if (= n 0)
    (stream-car s)
    (stream-ref (stream-cdr s) (- n 1))))

(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
        (begin 
               (set! result (proc))
               (set! already-run? true)
               result)
        result))))

(define-macro (delay x)
              `(memo-proc (lambda () ,x)))
;(define-macro (delay x)
;              `(lambda () ,x))


(define (force delayed-object) (delayed-object))

(define-macro (cons-stream a b)
              `(cons ,a (delay ,b)))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))

(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
    the-empty-stream
    (cons-stream
     (apply proc (map stream-car argstreams))
     (apply stream-map
            (cons proc (map stream-cdr argstreams))))))


(define (add-streams s1 s2)
  (stream-map + s1 s2))

(define (mul-streams s1 s2)
  (stream-map * s1 s2))

(define ones (cons-stream 1 ones))

(define integers (cons-stream 1
                              (add-streams ones integers)))

(define (s-times proc s n)
  (define (iter counter)
    (if (= n counter)
      'done
      (begin (p (proc s counter) )
             (iter (+ counter 1)))))
  (iter 0))

;(s-times stream-ref integers 4)


;----
; ; test
; 
; (define (stream-enumarate-interval low high)
;   (if (> low high)
;     the-empty-stream
;     (cons-stream
;       low
;       (stream-enumarate-interval (+ low 1) high))))
; 
; (trace stream-map)
; (trace add-streams)
; (trace mul-streams)
; 
; (define s1 (stream-enumarate-interval 1 10))
; (define s2 (stream-enumarate-interval 11 20))
; (define sm (mul-streams s1 s2))
; ;
; (p (stream-ref sm 1))
; (p (stream-ref sm 2))
; 
; (exit)


(p "\n-------------")
(p "p.194 fibs")

(define fibs
  (cons-stream 0
               (cons-stream 1
                            (add-streams (stream-cdr fibs)
                                         fibs))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor)) stream))
(define double (cons-stream 1 (scale-stream double 2)))

;----
; main
(define s (cons-stream 1 (add-streams s s)))

(p s)
(p (stream-ref s 1) )
(p (stream-ref s 2) )
(p (stream-ref s 3) )
(p (stream-ref s 4) )



(p "\n----------")
(p "p.195 ex.3.54")

; 1  2   3     4       5
; 1! 2!  3!    4!      5!
; 1 1*2 1*2*3 1*2*3*4 1*2*3*4*5
;
(define factorials
  (cons-stream 1
               (mul-streams factorials (add-streams ones integers))))
               ;(mul-streams factorials (cdr integers))))

(p (stream-ref factorials 0))
(p (stream-ref factorials 1))
(p (stream-ref factorials 2))
(p (stream-ref factorials 3))


(p "\n----------")
(p "p.195 問題3.55")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s) )))

(p "partial-sums: integer")
(s-times stream-ref (partial-sums integers) 5)
(p "partial-sums: double")
;(s-times stream-ref double 5)
(s-times stream-ref (partial-sums double) 5)


(p "\n----------")
(p "p.196 問題3.56")

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            (cond ((< s1car s2car)
                   (cons-stream s1car (merge (stream-cdr s1) s2)))
                  ((> s1car s2car)
                   (cons-stream s2car (merge s1 (stream-cdr s2))))
                  (else
                    (cons-stream s1car
                                 (merge (stream-cdr s1)
                                        (stream-cdr s2)))))))))


(define S (cons-stream 1
                       (merge (scale-stream S 2)
                              (merge (scale-stream S 3)
                                     (scale-stream S 5)))))


(s-times stream-ref S 10)

(p "\n----------")
(p "p.196 問題3.57")

(define counter 0)
(define (my-add a b)
  (set! counter (+ counter 1))
  (display "(")
  (display a)
  (display "+")
  (display b)
  (display "), ")
  (+ a b))

(define (add-streams2 s1 s2)
  (stream-map my-add s1 s2))

(define fibs2
  (cons-stream 0
               (cons-stream 1
                            (add-streams2 (stream-cdr fibs2)
                                         fibs2))))
(define (fib n)
  (display "Fib: ")
  (display n)
  (display ", RESULT: ")
  (display (stream-ref fibs2 n))
  (display ", COUNT: ")
  (p counter)
  (set! counter 0)
  )

(fib 1)
(fib 2)
(fib 3)
(fib 4)
(fib 5)

(p "\n----------")
(p "p.196 問題3.58")
; Q. (expand 1 7 10) が次々と生じる要素は何か
; A. 1 4 2 8 5 7 1 4 2 8 5 7 1 4 ...
;   #=> 1 を7 で割って、余りを7で割って。。。を繰り返してる。割り切れない。
; Q. (expand 3 8 10) が次々と生じる要素は何か
; A. 3 7 5 0 0 0 0 ...
;   #=> 3 を8 で割って、余りを8で割って。。。割り切れる。


(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(p "(expand 1 7 10)")
(s-times stream-ref (expand 1 7 10) 20)
(p "(expand 3 8 10)")
(s-times stream-ref (expand 3 8 10) 20)

(p "\n----------")
(p "p.196 問題3.59")

(p "p.197 問題3.59: a. integrate-series")

(define (integrate-series s)
  (stream-map / s integers))

(p "p.197 問題3.59: b. sin, cos")

(define (neg-streams s)
  (stream-map (lambda (x) (- x)) s))

(define cosine-series
  (cons-stream 1 (neg-streams (integrate-series sine-series))))

(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))

(p "\ncos -------")
(s-times stream-ref cosine-series 5)
(p "\nsin -------")
(s-times stream-ref sine-series 5)

(p "\n----------")
(p "p.197 問題3.60: mul-series")

; (a + i) * (b + j)
; =  ab + aj + bi + ij
; =  ab + (a + i)j + bi  ; (a + i) をそのままb のcdr にかけたもの
;                        ; b のcar をa のcdr にかける
(define (mul-series s1 s2)
    (cons-stream  (* (stream-car s1) (stream-car s2))
                  (add-streams (mul-series s1 (stream-cdr s2))
                               (scale-stream (stream-cdr s1) (stream-car s2)))))

(define one (add-streams (mul-series sine-series sine-series)
                        (mul-series cosine-series cosine-series)))

; ちゃんと1 になってる
(s-times stream-ref one 5)

(p "\n----------")
(p "p.197 問題3.61: invert-unit-series")

; 公式を手続きに落としただけ
(define (invert-unit-series s)
  (cons-stream 1
               (neg-streams (mul-series (stream-cdr s)
                                        (invert-unit-series s)))))

; cos とcos の逆数を見てみる。
; べき級数的に見れば合ってる…のか？
(p "\n cos")
(s-times stream-ref cosine-series 5)
(p "\n inverted cos")
(s-times stream-ref (invert-unit-series cosine-series) 5)

(p "\n----------")
(p "p.197 問題3.62: div-series")

; 逆数をかければよい。
(define (div-series s1 s2)
  (if (= (stream-car s2) 0)
    (error "ERROR: 0 division")
    (mul-series s1 (invert-unit-series s2))))

; tanX = sinX / cosX
(define tan (div-series sine-series cosine-series))
(define i 0) (until (= i 10)
       (format #t "~s " (stream-ref tan i ))
       (inc! i))
(format #t "\n")

