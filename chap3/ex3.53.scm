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

;----
; main
(define s (cons-stream 1 (add-streams s s)))

(p s)
(p (stream-ref s 1) )
(p (stream-ref s 2) )
(p (stream-ref s 3) )
(p (stream-ref s 4) )



(p "----------")
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


(p "----------")
(p "p.195 問題3.55")

(define (partial-sums s)
  (cons-stream (stream-car s)
               (add-streams (stream-cdr s) (partial-sums s) )))

(define (ref-s-times s n)
  (define (iter counter)
    (if (= n counter)
      'done
      (begin (p (stream-ref s counter) )
             (iter (+ counter 1)))))
  (iter 0))

;(ref-s-times integers 4)

(ref-s-times (partial-sums integers) 5)

