(require "../common.scm")

; op         | division-1 | division-2 | ...
; ------------------------------------------
; get-record | ...-proc   | ...
; get-salary | ...-proc   | ...
; ...

; a. 指定した従業員ファイルから、指定した従業員名のレコードを検索する
(define (get-record division-id employee-name)
  ((get division-id 'get-record) employee-name))

(define (install-division-1-package)
  ;; 内部手続き
  (define (get-record-proc employee-name)
    ; code to get record
    )

  ;; インタフェース
  (put 'division-1 'get-record get-record-proc)
  'done)

; file はdivision-id を含む


;
; b. 受け取ったレコードから、どの事業所のファイルからも給料情報が取り出せる
(define (get-salary record)
  ((get (division record) 'get-salary) (contents record)))

(define (install-division-1-package)
  ;; 内部手続き
  (define (get-salary-proc record)
    ; code to get salary
    )

  ;; インタフェース
  (put 'division-1 'get-salary get-salary-proc)
  'done)

; record はdivision-id を含む


;
; c. すべての事業所ファイルから、引数として与えられた従業員名 を含むレコードを返す
(define (find-employee-record employee-name division-list)
  (if ((null? division-list) '())
    (let (result (get-record employee-name (car division-list)))
      (if (null? result) 
        (find-employee-record employee-name (cdr division-list))
        result))))

;
; d. division の追加
;   - divisin-id をユニークにする
;   - インタフェースのput, install

