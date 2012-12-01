(require "../common.scm")
;(require "./gauss.scm")
;(require "./nikkei.scm")
;(require "./parallel.scm")

; p.180 問3.40
; ------------------------------------------------
; Q. 実行の結果となり得るx の可能性をすべて述べよ
; A.       100
;        1,000
;       10,000
;      100,000
;    1,000,000

(define x 10)
(parallel-execute (lambda () (set! x (* x x)))    ; p1
                  (lambda () (set! x (* x x x)))) ; p2

;   p1                |     p2
; f1-1: fetch x       |  f2-1: fetch x
; f1-2: fetch x       |  f2-2: fetch x
; c1-1: calc x * x    |  f2-3: fetch x
; s1-1: set x         |  c2-1: calc x * x * x
;                     |  s2-1: set x

;                     @                                 *
; (f1-1, f1-2, c1-1, s1-1, f2-1, f2-2, f2-3, c2-1,     s2-1)
;  10    10    100   100   100   100   100   1,000,000 1,000,000
;
;                           @                         *
; (f1-1, f1-2, c1-1, f2-1, s1-1, f2-2, f2-3, c2-1,   s2-1)
;  10    10    100   10    100   100   100   100,000 100,000
;
;                                 @                  *
; (f1-1, f1-2, c1-1, f2-1, f2-2, s1-1, f2-3, c2-1,  s2-1)
;  10    10    100   10    10    100   100   10,000 10,000
;
;                                       @            *
; (f1-1, f1-2, c1-1, f2-1, f2-2, f2-3, s1-1, c2-1,  s2-1)
;  10    10    100   10    10    10    100   1,000  1,000
;
;                                              *      @
; (f1-1, f1-2, f2-1, f2-2, f2-3, c2-1, c1-1, s2-1,  s1-1)
;  10    10    10    10    10    1,000 100   1,000  100
;
;                                 *                  @
; (f1-1, f2-1, f2-2, f2-3, c2-1,  s2-1,  f1-2,  c1-1,  s1-1)
;  10    10    10    10    1,000  1,000  1,000  10,000 10,000
;
;                            *                           @
; (f2-1, f2-2, f2-3, c2-1,  s2-1, f1-1, f1-2, c1-1,     s1-1)
;  10    10    10    1,000  1,000 1,000 1,000 1,000,000 1,000,000
;
; ------------------------------------------------
; Q. これらの可能性のうち、どれが残るか
; A. 1,000,000

;(define (make-serializer)
;  (let ((mutex (make-semaphore 1)))
;    (lambda (p)
;      (define (serialized-p . args)
;        (semaphore-wait mutex)
;        (let ((val (apply p args)))
;          (semaphore-post mutex)
;          val))
;      serialized-p)))

;(define s (make-serializer))
;(parallel-execute (s (lambda () (set! x (* x x))))
;                  (s (lambda () (set! x (* x x x)))))

;              @                   *
; (a1, b1, c1, d1, a2, b2, c2, d2, e2)
;  10  10  100 100 100 100 100 1,000,000 1,000,000
;
;              @                   *
; (a2, b2, c2, d2,   e2    a1,  b1,  c1,       d1, )
;  10  10  10  1,000 1,000 1000 1000 1,000,000 1,000,000

