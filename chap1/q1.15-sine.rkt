; q1.15
; sin x = x (x < 0.1)
; sin x = 3 * sin(x/3) - 4 * sin^3(x/3)

(define (cube x) (* x x x))
(define count 0)

(define (p x)
; (set! count (+ 1 count))
  (- (* 3 x) (* 4 (cube x))))

(define (sine-calc angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine-calc (/ angle 3.0)))))

(define (sine x)
  (set! count 0)
  (display (sine-calc x))
  (newline)
  (display count))
  
(trace sine-calc)
(trace p)
(sine 12.15)
;(sine 3.14) ; 1π rad = 180 angle

; a: how many times does p processed? => 5 times
; b: O(log a)
