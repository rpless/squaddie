#lang racket
(require (only-in "data.rkt" SPEED)
         2htdp/image)

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (struct-out squaddie) tick-squaddie draw-squaddie)

;; Data Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A squaddie is a (squaddie Complex Goal)
;; The complex is in world coordinates and the goal is Grid coordinates.
(struct squaddie (pos goal) #:transparent)

;; A Goal is oneof:
;; None
;; Location

;; A None is a #f
;; A Location is a Complex

;; Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Squaddie -> Squaddie
(define (tick-squaddie sq)
  (if (squaddie-goal sq)
      (move-squaddie sq)
      sq))

;; Squaddie -> Squaddie
(define (move-squaddie sq)
  (let ([pos (squaddie-pos sq)]
        [goal (squaddie-goal sq)])
    (if (<= (distance pos goal) SPEED)
        (squaddie goal #f)
        (let* ([npos (- goal pos)]
               [respos (/ npos (magnitude npos))])
          (squaddie (+ pos (* SPEED respos)) (squaddie-goal sq))))))

;; Complex Complex -> Real
(define (distance c1 c2)
  (sqrt (+ (sqr (- (real-part c2) (real-part c1)))
           (sqr (- (imag-part c2) (imag-part c1))))))

;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Squaddie Scene -> Scene
(define (draw-squaddie s scn)
  (let ([pos (squaddie-pos s)]
        [goal (squaddie-goal s)])                 
    (place-image (circle 5 'solid 'blue)
                 (real-part pos)
                 (imag-part pos)
                 (if goal
                     (place-image (triangle 10 'solid 'green)
                                  (real-part goal)
                                  (imag-part goal)
                                  scn)
                     scn))))