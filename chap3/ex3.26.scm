(require "../common.scm")

; p.159 ex3.26
; 上で実装した表を探すのに、レコードのリストを走査する必要がある # assoc の部分と思われる
; これは基本的に2.3.3 節の順序付けられていないリスト表現である。 # element-of-set?
; 巨大な表では、別のやり方で表を構成するほうがもっと効率的かもしれない。
; w2ws、値) のレコードが二進木を使って組織化する表の実装を述べよ。
; キーは何らかの方法で順序付けられるものとする。(2章のex2.66 と比べよ)
;

(define (make-tree entry left right)
  (list entry left right))

(define (make-leaf entry)
  (list entry '() '()))

(define (entry tree)
  (car tree))

(define (set-entry! tree entry)
  (set-car! tree entry))

(define (left-branch tree)
  (cadr tree))

(define (set-left-branch! tree lb)
  (set-car! (cdr tree) lb))

(define (right-branch tree)
  (caddr tree))

(define (set-right-branch! tree lb)
  (set-car! (cddr tree) lb))

; record
(define (make-record key data)
  (list key data))

(define (key-of-rec record)
  (car record))

(define (data-of-rec record)
  (cadr record))

; table
;   assocのcdr ダウンが木を辿る操作になる
(define (make-table)
  (let ((local-table (cons '*table* nil)))
    (define (tree-root)
      (cdr local-table))
    (define (set-tree-root! node)
      (set-cdr! local-table node))
    (define (node-lookup key node)
      (if (null? node)
        false
        (let ((cur-entry (entry node))
              (cur-key (key-of-rec (entry node))))
          (cond ((= key cur-key) cur-entry)
                ((< key cur-key)
                 (node-lookup
                   key
                   (left-branch node)))
                ((> key cur-key)
                 (node-lookup
                   key
                   (right-branch node)))))))
    (define (lookup key)
      (node-lookup key (cdr local-table)))

    (define (node-insert key data node)
      (let ((cur-entry (entry node))
            (cur-key (key-of-rec (entry node))))
        (cond ((= key cur-key)
               (set-entry!
                 node (make-record key data)))
              ((< key cur-key)
               (if (null? (left-branch node))
                 (set-left-branch!
                   node
                   (make-leaf
                     (make-record key data)))
                 (node-insert
                   key data (left-branch node))))
              ((> key cur-key)
               (if (null? (right-branch node))
                 (set-right-branch!
                   node
                   (make-leaf
                     (make-record key data)))
                 (node-insert
                   key data (right-branch node)))))))
    (define (insert! key data)
      (if (null? (tree-root))
        (set-tree-root!
          (make-leaf (make-record key data)))
        (node-insert key data (tree-root))))
    (define (print-table)
      (p local-table))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'print-table-proc) print-table)
            (else (error "Unknown operation -- TABLE" m))))
    (trace set-tree-root!)
    (trace node-insert)
    (trace insert!)
    (trace node-lookup)
    (trace lookup)
    (trace set-left-branch!)
    (trace set-right-branch!)
    dispatch))

(define my-t (make-table))
(define get (my-t 'lookup-proc))
(define put (my-t 'insert-proc!))
(define print-table (my-t 'print-table-proc))

(trace get)
(trace put)
(trace set-right-branch!)

(print-table)
(put 5 55)
(print-table)
(put 2 22)
(print-table)
(put 6 66)
(print-table)
(put 3 33)
(print-table)

(get 6)

(get 7)
(put 7 77)
(get 7)


