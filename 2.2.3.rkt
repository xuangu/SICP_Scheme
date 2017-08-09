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

#|
(flatmap (lambda (x)
             (filter (lambda (item)
                       (> item 5))
                     (list 1 2 6 7)))
           (list 10 11 12))
|#
(define (flatmap proc seq)
  (accumulate append
              '()
              (map proc seq)))

(define (integer-list n)
  (accumulate append
              '()
              (map (lambda (i)
                     (map (lambda (j) (list i j))
                          (enumerate-interval 1 (- i 1))))
                   (enumerate-interval 1 n))))
                   
(define (permutations-s s)
  (if (null? s)
      (list '())
      (flatmap (lambda (x)
                 (map (lambda (p) (cons x p))
                      (permutations (remove x s))))
               s)))

(define (remove x s)
  (filter (lambda (item)
            (not (= x item)))
          s))

;; define my-filter
(define (my-filter pred s)
  (cond ((null? s) '())
        ((pred (car s))
         (cons (car s) (my-filter pred (cdr s))))
        (else (my-filter pred (cdr s)))))

;; exercise 2.40
(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (prime-sum-pairs n)
  (map (lambda (item)
         (list (+ (car item)
                  (cadr item))
               (car item)
               (cadr item)))
       (filter (lambda (x)
                 (prime? (+ (car x)
                           (cadr x))))
               (unique-pairs n))))

(define (prime? x)
  (define (iter n i)
    (cond ((< (/ n 2) i) #t)
          ((= (remainder n i) 0) #f)
          (else (iter n (+ 1 i)))))
  (iter x 2))

;; exercise 2.41
(define (all-triples n)
  (flatmap (lambda (i)
             (map (lambda (jk) (list i (car jk) (cadr jk)))
                  (flatmap (lambda (j)
                             (map (lambda (k)
                                    (list j k))
                                  (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1)))))
           (enumerate-interval 1 n)))

(define (sum-equal-s-pairs s n)
  (filter (lambda (triple)
            (= s (+ (car triple)
                    (cadr triple)
                    (caddr triple))))
          (all-triples n)))

;; exercise 2.42
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions)
           (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (cons new-row rest-of-queens))

(define (safe? k positions)
  (iter-check (car positions)
              (cdr positions)
              1))

(define (iter-check new-row rest-queen-rows i)
  (if (null? rest-queen-rows)
      #t
      (let ((current-row (car rest-queen-rows)))
        (if
         (or (= new-row current-row)
             (= new-row (+ current-row i))
             (= new-row (- current-row i)))
         #f
         (iter-check
          new-row
          (cdr rest-queen-rows)
          (+ i 1))))))

(define (postion-xy position-row)
  (define col 9)
  (map (lambda (row)
         (begin
           (set! col (- col 1))
           (list row col)))
       position-row))

(define (make-diagonal-position-list position n)
  (let ((row (car position))
        (column (cadr position)))
    (append (map
             (lambda (arg1 arg2)
               (list (+ (car arg1)
                        (car arg2))
                     (+ (cadr arg1)
                        (cadr arg2))))
             (map (lambda (x)
                    (list row column))
                  (enumerate-interval 1 (- n column)))
             (map (lambda (x) (list x x))
                  (enumerate-interval 1 (- n column))))
            (map
             (lambda (arg1 arg2)
               (list (- (car arg1)
                        (car arg2))
                     (- (cadr arg1)
                        (cadr arg2))))
             (map (lambda (x)
                    (list row column))
                  (enumerate-interval 1 (- row 1)))
             (map (lambda (x) (list x x))
                  (enumerate-interval 1 (- row 1)))))))


(define (make-position-xy position-rows k)
  (map (lambda (row column)
         (list row column))
       position-rows
       (reverse (enumerate-interval k 8))))

#|
> (contains? (list (list 1 2) (list 3 4) (list 5 6)) (list 1 2))
#t
> (contains? (list (list 1 2) (list 3 4) (list 5 6)) (list 8 2))
|#
(define (contains? l e)
  (cond
    ((null? l) #f)
    ((same-position? (car l) e) #t)
    (else (contains? (cdr l) e))))

(define (same-position? p1 p2)
  (and (= (car p1)
          (car p2))
       (= (cadr p1)
          (cadr p2))))



;; exercise 2.43
