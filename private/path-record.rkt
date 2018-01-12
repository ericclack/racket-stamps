#lang typed/racket/base

(require typed/racket/class
         typed/racket/draw
         math/matrix
         racket/list
         data/queue
         "color-utils.rkt"
         "linalg-utils.rkt"
         "common.rkt"
         typed/racket/unsafe)

(unsafe-require/typed "priority-queue.rkt"
                      [ #:opaque Queue pqueue?]
                      [make-pqueue (-> (-> path Integer) Queue)]
                      [pqueue-add! (-> Queue path Void)]
                      [in-pqueue (-> Queue (Sequenceof path))]
                      [item-count (-> Queue Integer)])

(provide path
         PathRecord%
         path-record%)

(struct path ([points     : (Matrix Flonum)]
              [hue        : Flonum]
              [saturation : Flonum]
              [brightness : Flonum]
              [alpha      : Flonum]
              [z-order    : Integer])
  #:transparent)

(: path-bounding (-> path (values Flonum Flonum Flonum Flonum)))
(define (path-bounding P)

  (define mat (path-points P))
  (define N (matrix-num-cols mat))

  (: xs (Listof Flonum))
  (define xs (for/list ([i (range N)])
                       (matrix-ref mat 0 i)))

  (: ys (Listof Flonum))
  (define ys (for/list ([i (range N)])
                       (matrix-ref mat 1 i)))

  (values (apply min xs)
          (apply min ys)
          (apply max xs)
          (apply max ys)))

; Helper to apply color adjustments
(: set-brush-with-solid-color (-> (Instance Dc<%>) Flonum Flonum Flonum Flonum Void))
(define (set-brush-with-solid-color dc hue saturation brightness alpha)
  (define-values (r g b) (hsb->rgb hue saturation brightness))
  (define color (make-object color% r g b alpha))
  (send dc set-brush color 'solid))


(define-type PathRecord% (Class [get-bounding (-> (Values Flonum Flonum Flonum Flonum))]
                                [get-paths-count (-> Integer)]
                                [set-bounding (-> Flonum Flonum Flonum Flonum Void)]
                                [replay (-> (Instance Dc<%>) Void)]
                                [record-path (-> path Void)]))

(define path-record%
  (class object%

    (super-new)

    (: min-x Flonum)
    (: min-y Flonum)
    (: max-x Flonum)
    (: max-y Flonum)
    (define min-x 0.0)
    (define min-y 0.0)
    (define max-x 0.0)
    (define max-y 0.0)

    (: calc-bounding? Boolean)
    (define calc-bounding? #t)

    (define paths-queue (make-pqueue
                         (λ (p) (path-z-order p))))
    
    (: counter Integer)
    (define counter 0)

    (define/public (get-bounding) (values min-x min-y max-x max-y))

    (define/public (get-paths-count) (item-count paths-queue))

    (: set-bounding (-> Flonum Flonum Flonum Flonum Void))
    (define/public (set-bounding x1 y1 x2 y2)
      (set! min-x x1)
      (set! min-y y1)
      (set! max-x x2)
      (set! max-y y2)
      (set! calc-bounding? #f))

    (: replay (-> (Instance Dc<%>) Void))
    (define/public (replay dc)
      (: current-hue        Flonum)
      (: current-saturation Flonum)
      (: current-brightness Flonum)
      (: current-alpha      Flonum)
      (define current-hue        -1.0)
      (define current-saturation -1.0)
      (define current-brightness -1.0)
      (define current-alpha      -1.0)

      (define-values (w h) (send dc get-size))
      (define width (real->double-flonum w))
      (define height (real->double-flonum h))

      ; construct a transformation that translates and scales
      ; the bounding into the (0 0 sx sy) area
      (define b-width (- max-x min-x))
      (define b-height (- max-y min-y))
      (define x-factor (/ width  b-width))
      (define y-factor (/ height b-height))
      (define factor (min x-factor y-factor))
      (define trans (matrix* (translation-matrix (/ width -2.0)
                                                 (/ height -2.0))
                             (scaling-matrix factor factor)
                             (translation-matrix (/ b-width 2.0)
                                                 (/ b-height 2.0))
                             (translation-matrix min-x
                                                 min-y)))

      (for ([P (in-pqueue paths-queue)])
        (define hue (path-hue P))
        (define saturation (path-saturation P))
        (define brightness (path-brightness P))
        (define alpha (path-alpha P))

        ; whether the color was modified since last path
        (when (or (not (equal? current-hue hue))
                  (not (equal? current-saturation saturation))
                  (not (equal? current-brightness brightness))
                  (not (equal? current-alpha alpha)))
          (set-brush-with-solid-color dc hue saturation brightness alpha))

        ; transform the matrix according to "trans" and
        ; build a points (pairs of Flonums)
        (define mat (matrix* trans (path-points P)))
        (define N (matrix-num-cols mat))

        (: points (Listof (Pairof Flonum Flonum)))
        (define points (for/list ([i (range N)])
                         (cons (matrix-ref mat 0 i)
                               (matrix-ref mat 1 i))))

        (send dc draw-polygon points)

        (set! current-hue hue)
        (set! current-saturation saturation)
        (set! current-brightness brightness)
        (set! current-alpha alpha)))

    (: record-path (-> path Void))
    (define/public (record-path P)

      (when calc-bounding?
        (define-values (small-x small-y big-x big-y)
          (path-bounding P))

        (when (> big-x max-x) (set! max-x big-x))
        (when (> big-y max-y) (set! max-y big-y))
        (when (< small-x min-x) (set! min-x small-x))
        (when (< small-y min-y) (set! min-y small-y)))
      
      (pqueue-add! paths-queue P))

    ))

; ---------------------------------------------------------------

(module+ test
  (require typed/rackunit)

  (define epsilon .001)

  (test-case "path-record tests"
             
             (define pr (new path-record%))
             (define a-path (path (matrix [[ 0.0 -1.0  2.0]
                                           [-2.0  7.0  1.0]
                                           [ 1.0  1.0  1.0]])
                                  0.0 0.0 0.0 0.0 0))

             (check-eq? 0 (send pr get-paths-count))

             (send pr record-path a-path)
             (check-eq? 1 (send pr get-paths-count))
             
             (define-values (min-x min-y max-x max-y) (send pr get-bounding))
             (check-= -1.0 min-x epsilon)
             (check-= -2.0 min-y epsilon)
             (check-=  2.0 max-x epsilon)
             (check-=  7.0 max-y epsilon)
             
             (send pr record-path (path (matrix [[ 1.0  2.0  3.0]
                                                 [ 0.0 -4.0  1.0]
                                                 [ 1.0  1.0  1.0]])
                                        0.0 0.0 0.0 0.0 0))
             (check-eq? 2 (send pr get-paths-count))
             
             (set!-values (min-x min-y max-x max-y) (send pr get-bounding))
             (check-= -1.0 min-x epsilon)
             (check-= -4.0 min-y epsilon)
             (check-=  3.0 max-x epsilon)
             (check-=  7.0 max-y epsilon))

  (test-case "path function tests"
             
             (define a-path (path (matrix [[ 0.0 -1.0  2.0 -10.0]
                                           [-7.0  7.0  1.0  10.0]
                                           [ 1.0  1.0  1.0  1.0 ]])
                                  0.0 0.0 0.0 0.0 0))
             (define-values (min-x min-y max-x max-y) (path-bounding a-path))
             (check-= -10 min-x epsilon)
             (check-= -7 min-y epsilon)
             (check-=  2 max-x epsilon)
             (check-=  10 max-y epsilon))

  (test-case "paths with z-orders"

             ; Simple path creation with z-order
             (define pr (new path-record%))
             (define shape (matrix [[ 0.0 -1.0  2.0 -10.0]
                                    [-7.0  7.0  1.0  10.0]
                                    [ 1.0  1.0  1.0  1.0 ]]))
             (define a-path (path shape 0.0 0.0 0.0 0.0 5))
             (send pr record-path a-path)
             (check-eq? 1 (send pr get-paths-count))
             
             (define a-path2 (path shape 0.0 0.0 0.0 0.0 10))
             (send pr record-path a-path2)
             (check-eq? 2 (send pr get-paths-count))

             ; z-order determines sequence of paths
             ; TODO: the path with z-order of 10 should be first
))
