#lang s-exp stamps/lang

(define-shape row
  ((loop ([i 8])
         (circle [saturation 1.0]
                 [hue        0.0]
                 [brightness 0.5]
                 [translate (real->double-flonum (* i (random-real 0.98 1.0)))
                            (real->double-flonum (* i (random-real 0.98 1.0)))])))
  (row [rotate    60.0 ]
       [translate 5.0 5.0]
       [scale     0.9])
  (row [rotate    120.0]
       [translate 5.0 5.0]
       [scale     0.9])
)

(define-shape init
  (row [alpha -0.7]))

(maximum-render-cycles 100000)
(bounding '(-5.0 -10.0 15.0 10.0))
(start-shape init)