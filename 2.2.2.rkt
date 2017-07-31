#lang scheme
(define (my-list . items)
  (define (union x)
    (cond ((null? x) '())
          (else (cons (car x)
                      (union (cdr x))))))
  (union items))

;; count-leaves
(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

#|
> (list (list 1 2) (list 3 4))
((1 2) (3 4))
> (cons (list 1 2) (list 3 4))
((1 2) 3 4)
|#

;; exercise 2.25
;; (car (cdr (car (cdr (cdr (list 1 2 (list 5 7) 9))))))
;; (car (car (list (list 7))))
;; (cadr (cadr (cadr (cadr (car (cdr (car (cdr (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7))))))))))))))
;; (caadr (list 1 (list 1 2) 3))  ==>  1
;; (cadr (cons 1 (cons 2 3)))  ==>  2

;; exercise 2.26
;; => (1 2 3 4 5 6)
;; => ((1 2 3) 4 5 6)
;; => ((1 2 3) (4 5 6))

;; exercise 2.27
#|
(define (deep-reverse items)
  (cond ((null? items) '())
        ((not (pair? items)) items)
        (else (append 
               (deep-reverse (cdr (reverse-list (cdr items))))
               (cons (reverse-list (car items)) '())))))
|#
;; test case (deep-reverse (list (list 1 2) (list 3 4)))
;; test case
;; > (deep-reverse  (list (list 1 2) (list 3 4) (list 5 6) 7 8))
;; ==> (8 7 (6 5) (4 3) (2 1))
;; > (reverse-list (list (list 1 2) (list 3 4) (list 5 6) 7 8))
;; ==> (8 7 (5 6) (3 4) (1 2))
(define (deep-reverse items)
  (cond ((null? items) '())
        ((not (pair? items)) (cons items '()))
        (else (append 
               (deep-reverse (cdr items))
               (cons (reverse-list (car items)) '())))))


(define (reverse-list list)
  (cond ((null? list) '())
        ((not (pair? list)) list)
        (else (append (reverse-list (cdr list)) (cons (car list) '())))))

;; exercise 2.28
;; test case : (define x (list (list 1 2) (list 3 4)))
;; > x
;; ==> ((1 2) (3 4))
;; > (fringe x)
;; ==> (1 2 3 4)
;; > (fringe (list x x))
;; ==> (1 2 3 4 1 2 3 4)
(define (fringe tree)
  (cond ((null? tree) '())
        ((not (pair? tree)) (cons tree '()))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))
