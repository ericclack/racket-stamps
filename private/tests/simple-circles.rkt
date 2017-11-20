#lang s-exp stamps/lang

(define-shape row
  (circle [saturation 1  ]
        [hue        0  ]
        [brightness 0.5])
  (row [translate   0.5   0.5]
       [scale    0.8  ]
       [hue      2    ])
)

(maximum-render-cycles 10000)
(start-shape row)