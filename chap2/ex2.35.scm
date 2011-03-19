(require "../common.scm")

(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))








(p "* 木の葉を数え上げる手続き: enumerate-tree")
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(define tree1 (list 1 (list 2 (list 3 4)) 5) )
(p #`"* (enumerate-tree ,tree1)" )
(p (enumerate-tree tree1))

(p "問題2.35: count-leaves のアキュムレーションを再定義せよ")
; - 空リストのcount-leaves は0
; - 木x のcount-leaves はx のcar のcount-leaves 足すx のcdr のcount-leaves
; - 葉のcount-leaves は1
;
; 2.2 節のcount-leaves
(define (count-leaves-pre x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves-pre (car x))
                 (count-leaves-pre (cdr x))))))

; y のcdr は最終的にinitial が返って、それに1を足す。次は、それに1を足す。の繰り返す
; ** map を使わないバージョン
(define (count-leaves-nomap t)
  (accumulate (lambda (x y) (+ 1 y))
              0
              (enumerate-tree t)))


; ** map を使うバージョン
; map で部分木に対する葉の数を出しておいて、それがリストになって、最後にそれを足し合わせるだけ。
(define (count-leaves t)
  (accumulate (lambda (leaves total) (+ leaves total))
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                       (count-leaves sub-tree)
                       1))
                   t)))

;(trace enumerate-tree)
(trace accumulate)
(trace count-leaves)
;(define tree2 (list (list 1 2)
(define tree2 (list '()
                    (list 3 
                          (list 4 5))))
(p #`"* (count-leaves ,tree2)" )
(p (count-leaves tree2))

(p (count-leaves-pre tree2))

;* (count-leaves ((1 2) (3 (4 5))))
;CALL count-leaves ((1 2) (3 (4 5)))
; CALL count-leaves (1 2)              # 部分木を抽出
;  CALL accumulate #[proc] 0 (1 1)     # (1 2) は、左に1つ、に1つ、葉を持つ
;   CALL accumulate #[proc] 0 (1)
;    CALL accumulate #[proc] 0 ()
;    RETN accumulate 0
;   RETN accumulate 1
;  RETN accumulate 2
; RETN count-leaves 2
; CALL count-leaves (3 (4 5))
;  CALL count-leaves (4 5)
;   CALL accumulate #[proc] 0 (1 1)
;    CALL accumulate #[proc] 0 (1)
;     CALL accumulate #[proc] 0 ()
;     RETN accumulate 0
;    RETN accumulate 1
;   RETN accumulate 2
;  RETN count-leaves 2
;  CALL accumulate #[proc] 0 (1 2)
;   CALL accumulate #[proc] 0 (2)
;    CALL accumulate #[proc] 0 ()
;    RETN accumulate 0
;   RETN accumulate 2
;  RETN accumulate 3
; RETN count-leaves 3
; CALL accumulate #[proc] 0 (2 3)
;  CALL accumulate #[proc] 0 (3)
;   CALL accumulate #[proc] 0 ()
;   RETN accumulate 0
;  RETN accumulate 3
; RETN accumulate 5
;RETN count-leaves 5
;5

