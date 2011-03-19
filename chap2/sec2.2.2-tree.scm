(require "../common.scm")

(p "* scale-tree (cond)")
(define (scale-tree tree factor)
  (cond ((null? tree) nil)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(trace scale-tree)
(p (scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10))

(p "* scale-tree (map)")
(define (scale-tree2 tree factor)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (scale-tree2 sub-tree factor)
           (* sub-tree factor)))
       tree))

(trace scale-tree2)
(p (scale-tree2 (list 1 (list 2 (list 3 4) 5) (list 6 7))
            10))
