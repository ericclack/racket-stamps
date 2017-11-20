#lang typed/racket/base

; Rendering tooling

(require racket/class
         racket/draw
         "adjustments.rkt"
         "shape.rkt"
         "common.rkt"
         "path-record.rkt"
         typed/racket/unsafe)

(unsafe-require/typed data/queue
                      [ #:opaque Queue queue?]
                      [make-queue (-> Queue)]
                      [enqueue! (-> Queue ShapeRenderer Void)]
                      [dequeue! (-> Queue ShapeRenderer)]
                      [queue-empty? (-> Queue Boolean)]
                      [queue-length (-> Queue Integer)])

(provide maximum-render-cycles)

(unsafe-provide render-shape)

; Parameter that controls how many shapes to render
(define maximum-render-cycles (make-parameter 10000))


; Render a shape in a device context. Returns the number of shapes
; rendered
(: render-shape (-> Shape (Instance Dc<%>) Integer))
(define (render-shape shape dc)

  ; Phase 1: record paths
  ; ---------------------
  (define pr (new path-record%))
  (record-paths shape pr)

  ; Phase 2: replay paths
  ; ---------------------
  (send dc set-pen "black" 0 'transparent)
  (send dc set-smoothing 'smoothed)

  (send pr replay dc)

  (send pr get-paths-count))


; Record shape's paths in a path record
(: record-paths (-> Shape (Instance PathRecord%) Void))
(define (record-paths shape pr)
  (define renderers-queue (make-queue))
  (enqueue! renderers-queue (shape identity))
  
  (let render-loop ([renderer (dequeue! renderers-queue)]
                    [n 0])
    ; renderer is a shape, call it with our path record to capture its shape
    ; it will do this as long as it's not too small. It may return more
    ; renderers, i.e. sub-shapes, which we add to the queue, provided
    ; the shape has not become too small
    (let* ([current-shape-count (send pr get-paths-count)]
           [sub-shapes (renderer pr)]
           [new-shape-created (- (send pr get-paths-count)
                                 current-shape-count)])
      (when (> 0 new-shape-created)
        (for ([r sub-shapes])
             (enqueue! renderers-queue r))))
    
    (when (and (not (queue-empty? renderers-queue))
               (<= n (maximum-render-cycles)))
      (render-loop (dequeue! renderers-queue) (+ 1 n)))))
