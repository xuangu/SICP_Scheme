#lang scheme

; cons defination
(define (my-cons x y)
  (lambda (m)
    (cond ((= m 0) x)
          ((= m 1) y))))

(define (my-car x)
  (x 0))

(define (my-cdr x)
  (x 1))


; exercise-2.6
(define zero
  (lambda (f)
    (lambda (x)
      x)))

(define (add-1 n)
  (lambda (f)
    (lambda (x)
      (f ((n f) x)))))

(define one
  (lambda (f)
    (lambda (x)
      (f x))))

(define two
  (lambda (f)
    (lambda (x)
      (f (f x)))))

(define (print c)
  (display c)
  c)

#|(define church-add
    (lambda (m)
        (lambda (n)
            (lambda (f)
                (lambda (x)
                    (m f (n f x)))))))
|#

(define (church-func-call-times x)
  (display 'a))

(define (church-add m n)
  (lambda (f)
    (lambda (x)
      ((m f) ((n f) x)))))

(define (foo x)
  (display 'a))

#|
test case:
> ((two foo) 0)
aa
> (((church-add two one) foo) 0)
aaa
> (((church-add two one) church-func-call-times) 0)
aaa
>
|#

(define (foo1)
  (+ 2 3)
   100)



