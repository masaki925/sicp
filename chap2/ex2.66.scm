(require "../common.scm")

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

#| ; ---- comment start
; 順序付けられてない ver.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

; 順序付けられているリスト ver.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (car set)) true)
        ((< x (car set)) false)
        (else (element-of-set? x (cdr set)))))
|# ; ---- comment start


; 二分木バージョン
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))

(define set1 (adjoin-set 1 '()))
(p set1)

;----
; p.94 問2.66: レコードの集合がキーの数値で順序づけられている二進木で構造化されている場合のlookup

(define (make-record key data)
  (list key data))
(define (key record) (car record))
(define (data record) (cadr record))

; lookup 二分木バージョン
(define (lookup given-key set)
  (if (null? set)
    false
    (let ((cur-entry (entry set))
          (cur-key (key (entry set))))
      (cond ((= cur-key given-key) cur-entry)
            ((< given-key cur-key)
             (lookup given-key
                     (left-branch set)))
            ((> given-key cur-key)
             (lookup given-key
                     (right-branch set)))))))
(trace lookup)

(define record1 (make-record 1 'data1))
(define record2 (make-record 2 'data2))
(define record3 (make-record 3 'data3))
(define tree1 (make-tree record2 record1 record3 ))

(p (entry tree1))
(p (left-branch tree1))
(p (right-branch tree1))


