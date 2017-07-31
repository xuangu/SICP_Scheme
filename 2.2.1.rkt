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
(define us-coins (list 50 10 25 5 1))
(define uk-coins (list 100 50 10 20 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= 0 amount) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination coin-values))
            (cc (- amount
                   (first-denomination coin-values))
                coin-values)))))

(define (except-first-denomination coin-values)
  (cdr coin-values))

(define (first-denomination coin-values)
  (car coin-values))

(define (no-more? coin-values)
  (null? coin-values))


;;example change-count
(define (change-count account)
  (define (calc-count remainder-account kinds-of-coins)
    (cond ((= 0 remainder-account) 1)
          ((or (< remainder-account 0) (= kinds-of-coins 0)) 0)
          (else (+ (calc-count remainder-account (- kinds-of-coins 1))
                   (calc-count (- remainder-account (first-coin-value kinds-of-coins)) kinds-of-coins)))))
  (calc-count account 5))

(define (first-coin-value coin-kind-order)
  (cond ((= coin-kind-order 1) 50)
        ((= coin-kind-order 2) 25)
        ((= coin-kind-order 3) 10)
        ((= coin-kind-order 4) 5)
        ((= coin-kind-order 5) 1)))

;; exercise 2.20  这种想法不对
#|(define (same-parity . sequence)
  (define (combine parity-flag list)
    (cond ((null? list) '())
          ((same? parity-flag (car list))
           (cons parity-flag (combine parity-flag (cdr list))))
          (else (combine parity-flag (cdr list)))))
  (if (null? sequence) '()
      (combine (car sequence) (cdr sequence))))


(define (same? first next)
  (or (and (odd? first) (odd? next))
      (and (even? first) (even? next))))
|#

(define (same-parity . sequence)
  (cond ((odd? (car sequence))
               (filter-list odd? sequence))
         (else (filter-list even? sequence))))

(define (filter-list pred list)
  (cond ((null? list) '())
        ((pred (car list))
               (cons (car list)
                     (filter-list pred (cdr list))))
        (else (filter-list pred (cdr list)))))

;; exercise 2.21
;; recursive
(define (square-list-rec list)
  (if (null? list)
      '()
      (cons (square (car list))
            (square-list-rec (cdr list)))))

;; map
(define (square-list-map list)
  (map (lambda (x)
         (* x x))
       list))

(define (square x)
  (* x x))


;; exercise 2.2
;; test case 如下：(((() . 1) . 4) . 9)
(define (square-list-iter list)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (square (car things))))))
  (iter list '()))


#|
(define (square-list-iter list)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (square (car things))
                    answer))))
  (iter list '()))
|#


;; exercise 2.23
(define (my-for-each proc items)
  (define (proc-iter items)
    (if (null? items)
        'done
        (begin    ;; 注意begin过程的使用
          (proc (car items))
          (proc-iter (cdr items)))))
  (proc-iter items))
 


























