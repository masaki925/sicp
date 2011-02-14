(require srfi/27)
(define random random-integer)

(define (hoge x)
  (display x)
  (newline)
  (print-iter (- x 1)))

(define (print-iter x)
  (if (= 0 x)
      (display "end")
      (hoge x)))

;(trace print-iter)
(print-iter 4)
