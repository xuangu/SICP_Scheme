#lang scheme
(define (list-len l)
  (define (list-len-iter l count)
    (if (null? l)
        count
        (list-len-iter (cdr l) (+ 1 count))))
  (list-len-iter l 0))


(define (my-list-ref l n)
  (if (= 0 n) (car l)
      (my-list-ref (cdr l) (- n 1))))


(define (my-list-append l1 l2)
  (if (null? l1)
      l2
      (cons (car l1) (my-list-append (cdr l1) l2))))


;;exercise-2.17 last-pair
(define (last-pair l)
  (cons (my-list-ref l (- (list-len l) 1)) '()))


;;exercise-2.18 reverse
(define (reverse-list l)
  (reverse-list-iter l '()))

(define (reverse-list-iter remainder result)
  (if (null? remainder)
      result
      (reverse-list-iter (cdr remainder)
                         (cons (car remainder) result))))

;; 这个思想需要注意
(define (reverse-list-recur l)
  (if (null? l)
      '()
      (my-list-append (reverse-list-recur (cdr l)) (list (car l)))))

;;exercise-2.19


;;example change-count
(define (change-count account)
  (define (calc-count remainder-account kinds-of-coins)
    (cond ((= 0 remainder-account) 1)
          ((or (< remainder-account 0) (= kinds-of-coins 0)) 0)
          (else (+ (calc-count remainder-account (- kinds-of-coins 1))
                   (calc-count (- remainder-account (first-coin-value 1)) kinds-of-coins)))))
  (calc-count account 5))

(define (first-coin-value coin-kind-order)
  (cond ((= coin-kind-order 1) 50)
        ((= coin-kind-order 2) 25)
        ((= coin-kind-order 3) 10)
        ((= coin-kind-order 4) 5)
        ((= coin-kind-order 5) 1)))





      