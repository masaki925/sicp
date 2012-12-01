(require "../common.scm")

; p.159 問3.25
; 一次元、二次元を一般化し、値が任意個数のキーで格納され、値が異なればキーの個数も異なるかもしれぬ表を実装する方法を示せ。
; lookup とinsert! の手続きは、入力として表にアクセスするのに使うキーのリストをとるものとする

(define (make-table1)
  (let ((table (list '*table)))
    (define (lookup key)
      (let ((record (assoc key (cdr table))))
        (if record
          (cdr record)
          false))
      )
    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons key value)
                          (cdr table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    (trace lookup)
    (trace insert!)
    (trace dispatch)
    dispatch))

(trace make-table1)

#| ; comment begin ===========================
(define hoge (make-table1))
(define get (hoge 'lookup-proc))
(define put (hoge 'insert-proc!))

(put 'a 1)
(put 'b 2)
(get 'a)
|# ; comment end   ===========================

(define (make-table2)
  (let ((local-table (list '*table)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    (trace lookup)
    (trace insert!)
    (trace dispatch)
    dispatch))


(define (make-table0)
  (let ((local-table (list '*table*)))
    (define (lookup key-list table)
      (let ((candidate (assoc (car key-list) (cdr table))))
        (if candidate
          (if (number? (cdr candidate))
            (cdr candidate)
            (lookup (cdr key-list) candidate))
          false)))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    (trace my-assoc)
    (trace lookup)
    ;(trace insert!)
    (trace dispatch)
    dispatch))


(define hoge0 (make-table0))
(define get0 (hoge0 'lookup-proc))
(define put0 (hoge0 'insert-proc!))

(put0 '(a) 1)
(put0 '(b) 2)
(get0 'a)


; my-assoc
; - key-list それぞれについて、局所化されたtable を再帰的にたどる
;




    ; lookup ----------------------------------------
    (define (lookup key-1)
      (let ((record (assoc key-2 (cdr subtable))))
        (if record
          (cdr record)
          false)))

    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (cdr record)
              false))
          false)))

    (define (lookup key-1 key-2 key-3)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((subsubtable (assoc key-2 (cdr subtable))))
            (if subsubtable
              (let ((record (assoc key-3 (cdr subtable))))
                (if record
                  (cdr record)
                  false))
              false)))
        false))



    ; insert! ----------------------------------------
    (define (insert! key value)
      (let ((record (assoc key (cdr table))))
        (if record
          (set-cdr! record value)
          (set-cdr! table
                    (cons (cons key value)
                          (cdr table)))))
      'ok)

    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((record (assoc key-2 (cdr subtable))))
            (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)

    (define (insert! key-1 key-2 key-3 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
          (let ((sub-subtable (assoc key-2 (cdr subtable))))
            (if sub-subtable
              (let ((record (assoc key-3 (cdr sub-subtable))))
                (if record
                  (set-cdr! record value)
                  (set-cdr! sub-subtable
                            (cons (cons key-3 value)
                                  (cdr sub-subtable)))))
              (set-cdr! subtable
                        (cons (list key-2
                                    (cons key-3 value))
                              (cdr subtable)))))
          (set-cdr! local-table
                    (cons (list key-1
                                (cons key-2 value))
                          (cdr local-table)))))
      'ok)


    ; record: candidate
    ; subtable: sub-candidate
    ; local-table: table
    (define (insert! key-list value table)
      (let ((candidate (assoc (car key-list) (cdr table))))
        (if candidate
          (if (number? candidate) ; record?
            (set-cdr! candidate value)




