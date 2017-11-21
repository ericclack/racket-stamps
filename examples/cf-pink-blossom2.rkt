#lang s-exp stamps/lang

;; Original version by Momo https://contextfreeart.org/gallery2/index.html#design/137
;; Translated to Racket-Stamps by Eric Clack.

;; Creative Commons licensed: Creative Commons Attribution 3.0 Unported
;; https://creativecommons.org/licenses/by/3.0/

;; Changes: removed [skew] adjustments

(define-shape start
  (branch)
  (branch [flip 90]))

(define-shape shape
  [1 => (square)]
  [0.01 => (square)
        (circle [s 3 10])])

(define-shape branch
  [1 =>
     (shape [hue 10] [sat 0.5] [b 0.5])
     (branch [y 1] [s 0.995] [r 0.5])]
  [0.08 => (branch [flip 90])]
  [0.05 => (branch [s 0.85] [r 15])
        (branch [s 0.8] [r -15])]
  [0.02 => (start2 [r 45] [x 1.5] [alpha -0.2])
        (branch [s 0.8])])

(define-shape start2
  (start_ [sat 0.9] [hue 15] [b 0.8])
  (start_ [x -3.1] [flip -90] [hue 10] [sat 0.8] [b 0.8])
  (start2 [s 0.8] [x -0.3] [y 5] [flip 180]))

(define-shape start_
  (circle [s 3 10] [alpha -0.5])
  ;;(flower)
  (start_ [x 0.2] [r 9] [s 0.95] [b 0.05] [hue -1]))

(define-shape flower
  (petal [r -30] [y -0.37] [x 0.28] [b 0.1])
  (petal [flip -90] [x -1.08] [b 0.1])
  (petal [flip 150] [y -1.5] [x -0.9]))

(define-shape petal
  (triangle)
  (triangle [s 0.8] [b 1] [sat -1]))

(background '(1 .08 1))
(maximum-render-cycles 100000)
(minimum-shape-size 0.8)
(start-shape start)


