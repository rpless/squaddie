#lang racket
(require 2htdp/image "utilities/2vector.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (struct-out squaddie) tick-squaddie draw-squaddie)

;; Data Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A squaddie is a (squaddie Complex)
(struct squaddie (pos) #:transparent)

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define SPEED 5)

;; Update
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Squaddie Goal -> Squaddie
(define (tick-squaddie sq goal)
  (move-squaddie sq goal))

;; Squaddie -> Squaddie
(define (move-squaddie sq goal)
  (let ([pos (squaddie-pos sq)])
    (if (<= (distance pos goal) SPEED)
        (squaddie goal)
        (squaddie (+ pos (* SPEED (normalize (- goal pos))))))))

;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Squaddie Scene -> Scene
(define (draw-squaddie s scn)
  (let ([pos (squaddie-pos s)])
    (place-image (circle 5 'solid 'blue)
                 (real-part pos)
                 (imag-part pos)
                 scn)))