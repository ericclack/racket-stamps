#lang racket

;; Original version by Momo https://contextfreeart.org/gallery2/index.html#design/137
;; Translated to Racket-Stamps by Eric Clack.

;; Creative Commons licensed: Creative Commons Attribution 3.0 Unported
;; https://creativecommons.org/licenses/by/3.0/

;; Changes: removed [skew] adjustments

(require "../shape.rkt")
(require "../adjustments.rkt")
(require "../path-record.rkt")
(require racket/class)

;; A recursive shape...
(define-shape circles
  (circle)
  (circles [scale .9]))

(define pr (new path-record%))

;; Nothing in PR yet...

(let-values ([(min-x min-y max-x max-y)
              (send pr get-bounding)])
  (println min-x)
  (println max-x))

;; Now render a circle...
(define circles-renderer ((circles) identity))

;; Calling circles-renderer returns two renderers
;; the first is the circle, the second the recursive
;; call...
(let ([renderers (circles-renderer pr)])
  ((car renderers) pr))
  
;; Check to see what we have...
(let-values ([(min-x min-y max-x max-y)
              (send pr get-bounding)])
  (println min-x)
  (println max-x))
