(require "../common.scm")

; p.156 3.3.3 表の表現
;
; 2.3.3節(p.94), レコードをデータ構造として定義するとき、レコードに対応づけられるキーを検索するkey 選択手続きを用意しなければならない
; 2.4.3節(p.106), 演算と型の表から手続きを選択

(define (lookup key table)
  (let ((record (assoc key (cdr table))))
    (if record
      (cdr record)
      false)))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (insert! key value table)
  (let ((record (assoc key (cdr table))))
    (if record
      (set-cdr! record value)
      (set-cdr! table
                (cons (cons key value) (cdr table)))))
  'ok)

(define (make-table)
  (list '*table*))

#| ; comment begin ===========================
(trace make-table)
(trace insert!)
(trace lookup)

(define hoge (make-table))
(insert! 'a 1 hoge)
(insert! 'b 2 hoge)
(lookup 'b hoge)
|# ; comment end   ===========================

; p.157 二次元の表

(define (lookup2 key-1 key-2 table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          false))
      false)))

(define (insert!2 key-1 key-2 value table)
  (let ((subtable (assoc key-1 (cdr table))))
    (if subtable
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (set-cdr! record value)
          (set-cdr! subtable
                    (cons (cons key-2 value)
                          (cdr subtable)))))
      (set-cdr! table
                (cons (list key-1
                            (cons key-2 value))
                      (cdr table)))))
  'ok)

(define hoge2 (make-table))

(trace make-table)
(trace insert!2)
(trace lookup2)

(insert!2 'a 'b 1 hoge2)
(insert!2 'a 'c 2 hoge2)
(insert!2 'z 'b 3 hoge2)
(lookup2 'a 'b hoge2)

