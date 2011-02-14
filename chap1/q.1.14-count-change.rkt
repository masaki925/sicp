; q1.14
; - draw process tree
; - Space(n)
; - Step(n)

(define count 0)

(define (count-change amount)
  (set! count 0)
  (display (cc amount 5))
  (newline)
;  (display count)
  (newline))

(define (cc amount kinds-of-coins)
  (set! count (+ 1 count))
  (cond ((= amount 0) 1)
       ((or (< amount 0) (= kinds-of-coins 0)) 0)
       (else (+ (cc amount
                    (- kinds-of-coins 1))
                (cc (- amount
                       (first-denomination kinds-of-coins))
                    kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(count-change 11)

;-- Space(n)
; fact1. proportion of the depth of tree
; fact2. the deepest node is the case all amount is changed by smallest coins.
;
; (kinds-of-coins) + n => Space(n) = R(n)


;-- Step(n)
;** fact1. all process operates at least one step(return 0, return 1, plus sub-tree1 and sub-tree2)
; 
; f(n,5) = 1 + f(n,4) + f(n-50, 5)
; f(n,4) = 1 + f(n,3) + f(n-25, 4)
; f(n,3) = 1 + f(n,2) + f(n-10, 3)
; f(n,2) = 1 + f(n,1) + f(n-5, 2)
; f(n,1) = 1 + f(n,0) + f(n-1, 1)
; 
; f(n,0) = 1
;  => f(n,1) = 1 + 1 + f(n-1, 1)
;
; f(n-2), 1)
; …
; f(n-(n-4), 1) = 1 + 1 + f(n-(n-3),1)  ; f(11-7,1) = f(4,1) = 1 + 1 + f(3,1) = 1 + 1 + 7 = 9
; f(n-(n-3), 1) = 1 + 1 + f(n-(n-2),1)  ; f(11-8,1) = f(3,1) = 1 + 1 + f(2,1) = 1 + 1 + 5 = 7
; f(n-(n-2), 1) = 1 + 1 + f(n-(n-1),1)  ; f(11-9,1) = f(2,1) = 1 + 1 + f(1,1) = 1 + 1 + 3 = 5
; f(n-(n-1), 1) = 1 + 1 + f(n-n,1)      ; f(11-10,1) = f(1,1) = 1 + 1 + f(0,1) = 1 + 1 + 1 = 3
;               = 1 + 1 + f(0,1)
;
;** fact2. f(n,1) needs 2n+1 steps.
;   # f(11,1)= 2 * 11 + 1 = 23
;
;
; f(n,2) = 1 + f(n,1) + f(n-5, 2)
;        = 1 + 2n+1 + f(n-5,2)
;
; f(n-5,2) = 1 + 2n+1 + f(n-10,2)
; f(n-10,2) = 1 + 2n+1 + f(n-15,2)
; …
; about n/5 times.
;
;** fact3. f(n,2) needs n * n/5 = n^2/5 steps
; f(n,3) = 1 + f(n,2) + f(n-10 3)
; …
; about n/10 times.
;
;** fact4. f(n,3) needs n^2 * n/10 = n^3 steps
;
; we can say similar thing about f(n,4), f(n,5)
;
;*** Answer: Step(n) => R(n^5)
