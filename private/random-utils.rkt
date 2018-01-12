#lang typed/racket/base

(require (only-in math/base random-integer))

(provide random-real
         random-integer
         random-choice)

(: random-real (case-> (-> Flonum) (-> Real Flonum ) (-> Real Real Flonum)))
(define random-real
  (case-lambda
    [(min max)
     (real->double-flonum (+ (* (- max min) (random)) min))]
    [(max)
     (real->double-flonum (* max (random)))]
    [()
     (random)]))

; select a cdr value from one of the pairs with probability of the weight (car values).
(: random-choice (All (a) (->  (Listof (Pairof Flonum a)) a)))
(define (random-choice weighted-pairs)
  (define total (apply + (map (inst car Flonum Any) weighted-pairs)))
  (define theta (random-real (real->double-flonum total)))
  (let loop ([x 0.0]
             [wps weighted-pairs])
    (define wp (car wps))
    (define xx (+ x (car wp)))
    (if (<= theta xx)
        (cdr wp) ; choice made
        (loop xx (cdr wps))))) ; continue accumulating x and moving in wps list

(module+ test
  (require typed/rackunit
           racket/list)

  (for ([i (range 100)])
    (define r (random-real -10 -5))
    (check-true (and (<= -10 r) (<= r -5))))

  (for ([i (range 100)])
    (define r (random-real 5 10))
    (check-true (and (<= 5 r) (<= r 10))))


  (for ([i (range 100)])
    (define r (random-real -5 5))
    (check-true (and (<= -5 r) (<= r 5))))

  (for ([i (range 100)])
    (define r (random-real 5))
    (check-true (and (<= 0 r) (<= r 5)) ))

 (for ([i (range 100)])
    (define r (random-real))
    (check-true (and (<= 0 r) (<= r 1)))))

