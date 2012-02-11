(require "../common.scm")

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-list)
      (let ((record (assoc key-list (cdr local-table))))
        (if record
          (cdr record)
          false)))
    (define (insert! key-list value)
      (let ((record (assoc key-list (cdr local-table))))
        (if record 
          (set-cdr! record value)
          (set-cdr! local-table
                    (cons (cons key-list value)
                          (cdr local-table)))))
      'ok)
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            (else (error "Unknown operation -- TABLE" m))))
    dispatch))

(define hoge (make-table))
(define get (hoge 'lookup-proc))
(define put (hoge 'insert-proc!))

(trace get)
(trace put)

(put '(a b c) 1)
(get '(a b c))

(put '(a) 10)
(get '(a))
(get '(a b c))

