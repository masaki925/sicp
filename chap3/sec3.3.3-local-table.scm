(require "../common.scm")

(define (make-table)
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

(trace make-table)

(define operation-table (make-table))
(trace operation-table)

(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))

(put 'a 'b 1)
(put 'a 'c 2)
(put 'z 'b 3)

(get 'z 'b)


(define operation-table2 (make-table))
(trace operation-table2)

(define get2 (operation-table2 'lookup-proc))
(define put2 (operation-table2 'insert-proc!))

(put2 'z 'b 9)

(get2 'z 'b)
(get 'z 'b)


