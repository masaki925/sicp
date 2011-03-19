(require "../common.scm")

(p "
p.66: 並びの演算
- プロセスのある段から次へ流れる「信号」に集中する。
- この信号をリストで表現できるなら、格段での処理を実装するのにリスト演算を使うことができる
")

(p "* 写像")
(p (map square (list 1 2 3 4 5)))
(p "* フィルタ")
(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(p (filter odd? (list 1 2 3 4 5)))
(p "* アキュムレーション")
(define (accumulate op initial sequence)
  (if (null? sequence)
    initial
    (op (car sequence)
        (accumulate op initial (cdr sequence)))))
(p (accumulate + 0 (list 1 2 3 4 5)))
(p (accumulate * 1 (list 1 2 3 4 5)))
(p (accumulate cons nil (list 1 2 3 4 5)))


(p "* 要素の並びを数え上げる")

(define (enumerate-interval low high)
  (if (> low high)
    nil
    (cons low (enumerate-interval (+ low 1) high))))

(p (enumerate-interval 2 7))

(p "* 木の葉を数え上げる")
(define (enumerate-tree tree)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))
(p (enumerate-tree (list 1 (list 2 (list 3 4)) 5)))

(p "sum-odd-squares2 (信号処理方式)")

(define (sum-odd-squares2 tree)
  (accumulate +
              0
              (map square
                   (filter odd?
                           (enumerate-tree tree)))))

(define tree2 (list 1 (list 2 (list 3 4)) 5))
(p (sum-odd-squares2 tree2))


(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                (fib (- n 2))))))

(p "even-fibs2 (信号処理方式)")
(define (even-fibs2 n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
(p (even-fibs2 4))

(p "
プログラムを並びの演算として表す価値
- 部品化されたプログラム設計 = 比較的独立な部品を組み合わせて作り上げる設計を可能とすること


- 標準部品のライブラリ
- 部品を柔軟に接続するための公認インタフェース

を用意することで、部品化された設計を奨励する

標準部品化された組み立ては、複雑さを制御する強力な戦略である
実際の信号処理の応用では、設計者はフィルタや変換器の標準化された部品群の中から選択した素子を数珠つなぎにすることでシステムを構築する
=
並びの演算は、取り替え可能な標準プログラム素子のライブラリを用意する。
例えばsum-odd-squares とeven-fibs の部品を、最初のn+1 個のFibonacci 数の2乗のリストを作るのに再利用できる
")

(define (list-fib-squares n)
  (accumulate cons
              nil
              (map square
                   (map fib
                        (enumerate-interval 0 n)))))
(p "* (list-fib-squares 10)")
(trace accumulate)
(trace list-fib-squares)
(p (list-fib-squares 10))

(p "並びの中の奇数の2乗の積の計算")
(define (product-of-squares-of-odd-elements sequence)
  (accumulate *
              1
              (map square
                   (filter odd? sequence))))

(p (product-of-squares-of-odd-elements (list 1 2 3 4 5)))

(p "慣習的データ処理も、並びの演算を使って形式化できる")
(p "
* 個人レコードの並びがあり、最高収入のプログラマの給料を見つける: salary-of-highest-paid-programmer
- レコードから給料を買えす選択子 salary
- レコードがプログラマのものかをテストする述語 programmer?
")

(define (salary-of-highest-paid-programmer records)
  (accumulate max
              0
              (map salary
                   (filter programmer? records))))


(p "
-リストとして実装した並び
=> 処理部品を組み合わせるのを可能にする公認インタフェースとして役立つ

-構造を並びとして一様に表現する
=> プログラムのデータ構造依存性を、少数の並びの演算の中に局所化できる

これらを変更して並びの別の表現の実験が出きるが、プログラム全体の設計には手をつけないで済む。
")

