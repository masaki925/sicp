(require "../common.scm")

;p.58 両替問題をリストを使って

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

; 手続き～を定義せよ
(define (first-denomination coins) (car coins))
(define (except-first-denomination coins) (cdr coins))
(define (no-more? coins) (null? coins))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
          (+ (cc amount
                 (except-first-denomination coin-values))
             (cc (- amount
                    (first-denomination coin-values))
                 coin-values)))))

; リスト coin-values の順は, cc の答えに影響があるか. なぜか.
(mydisplay "* normal order----")
(mydisplay "(cc 100 us-coins)")
(trace cc)
(mydisplay (cc 100 us-coins))
(mydisplay "(cc 50 uk-coins)")
(mydisplay (cc 50 uk-coins))

(mydisplay "--------------------")
(mydisplay "* reversed order----")
(mydisplay "(cc 100 (reverse us-coins))")
(mydisplay (cc 100 (reverse us-coins)))
(mydisplay "(cc 50 (reverse uk-coins))")
(mydisplay (cc 50 (reverse uk-coins)))


; イメージ
;(cc 100 us-coins) #=> 292
;
;                 (cc 100 (50 25 10 5 1))
;               (cc 100 (25 10 5 1))
;             (cc 100 (10 5 1))
;           (cc 100 (5 1))
;
;                        (cc 95 (5 1))               ; 5を1枚使うパターン
;  (cc 100 (1))                        (cc 90 (5 1)) ; 5を2枚使うパターン
;                 (cc 95 (1))                        ; …  20通り


; イメージ
;(cc 100 us-coins) #=> 292
;
;                 (cc 100 (1 5 10 25 50))
;               (cc 100 (5 10 25 50))
;             (cc 100 (10 25 50))
;           (cc 100 (25 50))
;
;                        (cc 75 (25 50))                ; 25を1枚使うパターン
;  (cc 100 (50))                        (cc 50 (25 50)) ; 25を2枚使うパターン
;                 (cc 75 (50))                          ; …  4通り


;---------------------------------------
;p.22, 両替の問題
(define (old-count-change amount) (old-cc amount 5))

(define (old-first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (old-first-denomination2 kinds-of-coins)
  (cond ((= kinds-of-coins 1) 50)
        ((= kinds-of-coins 2) 25)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 5)
        ((= kinds-of-coins 5) 1)))

(define (old-cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (old-cc amount
                         (- kinds-of-coins 1))
                 (old-cc (- amount
                            ;(old-first-denomination kinds-of-coins))
                            (old-first-denomination2 kinds-of-coins))
                         kinds-of-coins)))))

;(mydisplay "(old-count-change 100)")
;(mydisplay (old-count-change 100))


