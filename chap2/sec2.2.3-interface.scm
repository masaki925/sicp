(require "../common.scm")

(p "
p.65: 2.2.3 公認インタフェースとしての並び

* データ構造を扱う強力な設計原理
- データ抽象
-- データ表現の細部にまで巻き込まれずにプログラムが設計できる
-- いろいろな表現を試す柔軟さを保つ

- 公認インタフェースの利用

----

1.3節、公開手続きとして実装したプログラム抽象が、数値データを扱うプログラムの共通パターンを取り込む様子

- 級数の総和
-- a から b までの整数の和
-- 与えられた範囲の整数の3乗の和
-- 1/1*3 + 1/5*7 + 1/9*11 + ... の和

=> 手続きを引数にとる手続き

")

(p "引数として木をとり、奇数である葉の2乗の和を計算する(2.2.2 節のcount-leaves に類似) ")

(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                  (sum-odd-squares (cdr tree))))))

(trace sum-odd-squares)
(define tree1 (cons (list 1 2) (list 3 4)))
(p #`"* (sum-odd-squares ,tree1)\n")
(p (sum-odd-squares tree1))

(p "与えられた整数n より小さいか等しいk に対して、偶数のFibonacci数Fib(n) のリストを作る")
(define (fib n)
  (cond ((= n 0) 0)
        ((= n 1) 1)
        (else (+ (fib (- n 1))
                (fib (- n 2))))))

(define (even-fibs n)
  (define (next k)
    (if (> k n)
      nil
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (trace next)
  (next 0))

(p "* (even-fibs 10)")
(p (even-fibs 10))

(p "
--------------
- 数珠つなぎの段を流れる信号
-- 数え上げ(enumerator)
-- フィルタ(filter)
-- 変換器
-- アキュムレータ(accumulator)

※ 今までの書き方では、信号処理構造を示せていない。


並びの演算
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
(sum-odd-squares2 tree2)


(p "even-fibs2 (信号処理方式)")
(define (even-fibs2 n)
  (accumulate cons
              nil
              (filter even?
                      (map fib
                           (enumerate-interval 0 n)))))
(p (even-fibs2 4))

