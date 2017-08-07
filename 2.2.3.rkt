#lang scheme
;; 也称为fold-right,事实上op的应用顺序是从右向左的。先对最后一个元素进行op操作,然后前一个,一直回到第一个
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ 1 low) high))))

(define (enumerate-tree tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (sum-odd-squares tree)
  (accumulate +
              0
              (map (lambda (x) (* x x))
                   (filter odd? (enumerate-tree tree)))))

;; exercise 2.33
(define (accumulate-map proc sequence)
  (accumulate (lambda (x y)
                (cons (proc x) y))
              '()
              sequence))

#|
> (accumulate-append (list 1 2 3) (list 4 5 6))
(1 2 3 4 5 6)
> (cons 1 (cons 2 (cons 3 (list 4 5 6))))
(1 2 3 4 5 6)
|#
(define (accumulate-append seq1 seq2)
  (accumulate cons
              seq2
              seq1))

(define (length sequence)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              sequence))
              
;; exercise 2.34
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                (+ this-coeff
                   (* x higher-terms)))
              0
              coefficient-sequence))

;; exercise 2.35 (重点关注）
(define (count-leaves-enum t)
  (accumulate (lambda (x y)
                (+ 1 y))
              0
              (enumerate-tree t)))

;; (count-leaves-map (list (list 1 2) 3 (list 3 5 65)))
(define (count-leaves-map t)
  (accumulate +
              0
              (map (lambda (sub-tree)
                     (if (pair? sub-tree)
                         (count-leaves-map sub-tree)
                         1))
                   t)))

;; exercise 2.36
#|
(accumulate-n +
                0
                (list (list 1 2 3)
                      (list 4 5 6)
                      (list 7 8 9)
                      (list 10 11 12)))
|#
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

;; exercise 2.37
#|
(matrix-*-vector (list (list 1 2)
                         (list 3 4))
                   (list (list 5 6)
                         (list 7 8)))
|#
(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (col)
         (dot-product col v))
       m))

#|
(transpose (list (list 1 2 3)
                   (list 4 5 6)
                   (list 7 8 9)
                   (list 11 12 13)))
((1 4 7 11) (2 5 8 12) (3 6 9 13))
|#
(define (transpose mat)
  (accumulate-n cons
                '()
                mat))
#|
(matrix-*-matrix (list (list 1 2)
                         (list 3 4)
                         (list 5 6))
                   (list (list 7 8 9)
                         (list 10 11 12)))
|#
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (mj)
           (map (lambda (colsj)
                        (dot-product colsj mj))
                cols))
         m)))


;; exercise 2.38
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))
#|
> (define fold-right accumulate)
> (fold-right / 1 (list 1 2 3))
1 1/2
> (fold-left / 1 (list 1 2 3))
1/6
> (fold-right list '() (list 1 2 3))
(1 (2 (3 ())))
> (fold-left list '() (list 1 2 3))
(((() 1) 2) 3)
|#
    
;; exercise 2.39
(define fold-right accumulate)

(define (reverse-use-fold-right sequence)
  (fold-right (lambda (x y)
                (append y (list x)))
              '()
              sequence))

(define (reverse-use-fold-left sequence)
  (fold-left (lambda (x y)
               (append y (list x)))
             '()
             sequence))

  