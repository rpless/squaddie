#lang racket
(require (only-in "data.rkt" SPEED)
         2htdp/image)

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (struct-out squaddie) tick-squaddie draw-squaddie)

;; Data Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A squaddie is a (squaddie Complex)
(struct squaddie (pos) #:transparent)

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
        (let* ([npos (- goal pos)]
               [respos (/ npos (magnitude npos))])
          (squaddie (+ pos (* SPEED respos)))))))

;; Complex Complex -> Real
(define (distance c1 c2)
  (sqrt (+ (sqr (- (real-part c2) (real-part c1)))
           (sqr (- (imag-part c2) (imag-part c1))))))

;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Squaddie Scene -> Scene
(define (draw-squaddie s scn)
  (let ([pos (squaddie-pos s)])
    (place-image (circle 5 'solid 'blue)
                 (real-part pos)
                 (imag-part pos)
                 scn)))