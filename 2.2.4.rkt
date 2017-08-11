#lang scheme
;; test address: http://www.biwascheme.org/demo/pictlang.html

(define (right-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

(define (corner-split painter n)
  (if (= 0 n)
      painter
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

;; exercise 2.44
(define (up-split painter n)
  (if (= 0 n)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))

(define (square-of-four top-left top-right bottom-left bottom-right)
  (lambda (painter)
    (let ((top (beside (top-left painter) (top-right painter)))
          (bottom (beside (bottom-left painter) (bottom-right painter))))
      (below bottom top))))

(define (flipped-pairs painter)
  (let ((combine4 (square-of-four identity flip-vert
                                  identity flip-vert)))
    (combine4 painter)))

(define (square-limit-h painter n)
  (let ((combine4 (square-of-four flip-horiz identity
                                  rotate180 flip-vert)))
    (combine4 (corner-split painter n))))

;; exercise 2.45
(define (split big-combiner small-combiner)
  (lambda (painter n)
    (let ((smaller ((split big-combiner small-combiner) painter (- n 1))))
      (big-combiner painter (small-combiner smaller)))))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v)
                           (edge1-frame frame))
               (scale-vect (ycor-vect v)
                           (edge2-frame frame))))))

(define (make-vect x y)
  (cons x y))

(define (xcor-vect v)
  (car v))

(define (ycor-vect v)
  (cdr v))

(define (add-vect v1 v2)
  (make-vect
   (+ (xcor-vect v1)
      (xcor-vect v2))
   (+ (ycor-vect v1)
      (ycor-vect v2))))

(define (sub-vect v1 v2)
  (make-vect
   (- (xcor-vect v1)
      (xcor-vect v2))
   (- (ycor-vect v1)
      (ycor-vect v2))))

(define (scale-vect vect s)
  (make-vect
   (* s (xcor-vect vect))
   (* s (ycor-vect vect))))

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (origin-frame frame)
  (car frame))

(define (edge1 frame)
  (cadr frame))

(define (edge2 frame)
  (caddr frame))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line
        ((frame-coord-map frame) (start-segment segment))
        ((frame-coord-map frame) (end-segment segment))))
     segment-list)))

;; exercise 2.48
(define (make-segment start end)
  (cons start end))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

;; exercise 2.49
(define frame-painter
  (segments->painter (list (make-segment
                            (make-vect 0 0)
                            (make-vect 1 0))
                           (make-segment
                            (make-vect 1 0)
                            (make-vect 1 1))
                           (make-segment
                            (make-vect 0 0)
                            (make-vect 0 1))
                           (make-segment
                            (make-vect 0 1)
                            (make-vect 1 1)))))

(define fork-painter
  (segments->painter (list (make-segment
                            (make-vect 0.0 0.0)
                            (make-vect 1.0 1.0))
                           (make-segment
                            (make-vect 1.0 0.0)
                            (make-vect 0.0 1.0)))))

(define (transform-painter painter origin edge1 edge2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m edge1) new-origin)
                  (sub-vect (m edge2) new-origin)))))))

(define (flip-vect painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (shrink-to-upper-right painter)
  (tansform-painter painter
                    (make-vect 0.5 0.5)
                    (make-vect 0.1 0.5)
                    (make-vect 0.5 1.0)))

(define (rotate90 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

(define (squash-inwards painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

(define (beside painter1 painter2)
  (let ((split-pointer (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter painter1
                              (make-vect 0.0 0.0)
                              split-pointer
                              (0.0 1.0)))
          (paint-right
           (transform-painter painter2
                              split-pointer
                              (make-vect 1.0 0.0)
                              (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

(define (flip-horiz painter)
  (transform-painter painter
                    (make-vect 1.0 0.0)
                    (make-vect 0.0 0.0)
                    (make-vect 1.0 1.0)))

(define (rotate180 painter)
  (transform-painter painter
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 1.0)
                     (make-vect 1.0 0.)))

(define (rotate270 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0)
                     (make-vect 0.0 0.0)
                     (make-vect 1.0 1.0)))

;; exercise 2.51
(define (below painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((bottom (transform-painter painter1
                                     (make-vect 0.0 0.0)
                                     (make-vect 1.0 0.0)
                                     split-point))
          (top (transform-painter painter2
                                  split-point
                                  (make-vect 1.0 0.5)
                                  (make-vect 0.0 1.0))))
      (lambda (frame)
        (bottom frame)
        (top frame)))))

;; 本解法实质上要搞清楚线性代数的变换规则就可以了，需要一定的线性代数矩阵运算基础
(define (below painter1 painter2)
  (lambda (frame)
    ((flip-horiz
      (rotate90
       (beside
        (rotate270
         (flip-horiz painter1))
        (rotate270
         (flip-horiz painter2)))))
     frame)))
          
      




                     



              
      
          