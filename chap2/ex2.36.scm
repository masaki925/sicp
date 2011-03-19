(require "../common.scm")

(p "問題2.36:
- 第3引数としてすべてが同数の要素からなる並びの並びをとる
- アキュムレーションとして指定した手続きを、並びのすべての第1要素、第2要素、というふうに作用させ、結果の並びを返す

(define s ((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
とすると、
(accumulate-n + 0 s)
#=> (22 26 30)
    = ((1 + 4 + 7 + 10) (2 + 5 + 8 + 11) (3 + 6 + 9 + 12))

")

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
    nil
    (cons (accumulate op init (map car seqs))
          (accumulate-n op init (map cdr seqs)))))

(trace accumulate)
(trace accumulate-n)
(define s (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))
(p #`"(accumulate-n + 0 ,s)" )
(p (accumulate-n + 0 s))

 ; accumulate  : 最終的にはinitial が返る
 ; accumulate-n: 最終的にはnil が返る
 ;
 ;** (map car seqs)
 ; gosh> (define t (list (list 1 2) (list 3 4)))
 ; t
 ; gosh> (map car t)
 ; (1 3)
 ;
 ;
;(accumulate-n + 0 ((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
;
;** 要素の1番め
;CALL accumulate-n #[proc] 0 ((1 ...) (4 ...) (7 8 ...) (10 11 12))
; CALL accumulate #[proc] 0 (1 4 7 10)
;  CALL accumulate #[proc] 0 (4 7 10)
;   CALL accumulate #[proc] 0 (7 10)
;    CALL accumulate #[proc] 0 (10)
;     CALL accumulate #[proc] 0 ()
;     RETN accumulate 0
;    RETN accumulate 10
;   RETN accumulate 17
;  RETN accumulate 21
; RETN accumulate 22
;** 要素の2番め
; CALL accumulate-n #[proc] 0 ((2 ...) (5 ...) (8 9) (11 12))
;  CALL accumulate #[proc] 0 (2 5 8 11)
;   CALL accumulate #[proc] 0 (5 8 11)
;    CALL accumulate #[proc] 0 (8 11)
;     CALL accumulate #[proc] 0 (11)
;      CALL accumulate #[proc] 0 ()
;      RETN accumulate 0
;     RETN accumulate 11
;    RETN accumulate 19
;   RETN accumulate 24
;  RETN accumulate 26
;** 要素の3番め
;  CALL accumulate-n #[proc] 0 ((3) (6) (9) (12))
;   CALL accumulate #[proc] 0 (3 6 9 12)
;    CALL accumulate #[proc] 0 (6 9 12)
;     CALL accumulate #[proc] 0 (9 12)
;      CALL accumulate #[proc] 0 (12)
;       CALL accumulate #[proc] 0 ()
;       RETN accumulate 0
;      RETN accumulate 12
;     RETN accumulate 21
;    RETN accumulate 27
;   RETN accumulate 30
;** 要素の4番め
;   CALL accumulate-n #[proc] 0 (() () () ())
;   RETN accumulate-n ()
;** 最後のcons
;  RETN accumulate-n (30)
; RETN accumulate-n (26 30)
;RETN accumulate-n (22 26 30)
;(22 26 30)

