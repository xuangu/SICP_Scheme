#lang scheme

(define (my-sqrt x)
  (newton-method (lambda (y)
            (- x (square y)))
          1))

(define (newton-method f guess)
  (fixed-point (newton-transform f) guess))

(define (newton-transform f)
  (lambda (x)
    (- x
       (/ (f x)
          ((deriv f) x)))))

(define (deriv f)
  (lambda (x)
    (/ (- (f (+ x dx))
          (f x))
       dx)))

(define dx 0.0000001)

(define (fixed-point f guess)
  (define tolerance 0.000001)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try guess))

(define (abs x)
  (if (> x 0)
      x
      (- x)))

(define (square x)
  (* x x))

  