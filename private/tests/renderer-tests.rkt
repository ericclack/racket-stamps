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


(define pr (new path-record%))

;; Nothing in PR yet...

(let-values ([(min-x min-y max-x max-y)
              (send pr get-bounding)])
  (print min-x)
  (print max-x))

;; Now render a circle...
(define circle-renderer ((circle) identity))

;; Render the single circle and return empty set...
(circle-renderer pr)

;; Check to see what we have...
(let-values ([(min-x min-y max-x max-y)
              (send pr get-bounding)])
  (print min-x)
  (print max-x))
