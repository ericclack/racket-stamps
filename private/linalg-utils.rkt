#lang typed/racket/base

;; Linear algebra combinators

(require math/matrix
         racket/math)

(provide rotation-matrix
         scaling-matrix
         translation-matrix
         reflection-matrix
         shear-matrix)

(: rotation-matrix (-> Flonum (Matrix Flonum)))
(define (rotation-matrix deg)
  (define rad (degrees->radians deg))
  (matrix [[(cos rad)   (- (sin rad)) 0.0]
           [(sin rad)   (cos rad)     0.0]
           [0.0         0.0           1.0]]))

(: scaling-matrix (-> Flonum Flonum (Matrix Flonum)))
(define (scaling-matrix sx sy)
  (matrix [[sx   0.0  0.0]
           [0.0  sy   0.0]
           [0.0  0.0  1.0]]))

(: translation-matrix (-> Flonum Flonum (Matrix Flonum)))
(define (translation-matrix dx dy)
  (matrix [[1.0  0.0  (- dx)]
           [0.0  1.0  (- dy)]
           [0.0  0.0  1.0 ]]))

(: reflection-matrix (-> Flonum (Matrix Flonum)))
(define (reflection-matrix deg)
  (define rad (degrees->radians deg))
  (define x (cos rad))
  (define y (sin rad))
  (matrix [[(- (sqr x) (sqr y)) (* 2.0 x y)         0.0]
           [(* 2.0 x y)         (- (sqr y) (sqr x)) 0.0]
           [0.0                 0.0                 1.0]]))

(: shear-matrix (-> Flonum Flonum (Matrix Flonum)))
(define (shear-matrix x y)
  (matrix [[1.0  x    0.0]
           [y    1.0  0.0]
           [0.0  0.0  1.0]]))
