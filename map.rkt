#lang racket
(require (only-in "data.rkt" WIDTH HEIGHT CELL-SIZE)
         2htdp/image)
(provide WORLD-SCENE)

;; Squaddie Map Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module contains the map for the game.

;; Integer Integer Scene -> Scene
(define (draw-cell width height scn)
  (place-image (overlay (circle 1 'solid 'red)
                        (rectangle CELL-SIZE CELL-SIZE 'outline 'purple))
               (grid-x->pixel width)
               (grid-y->pixel height)
               scn))

;; Integer -> Integer
(define (grid-x->pixel x)
  (+ (* CELL-SIZE x) (/ CELL-SIZE 2)))

;; Integer -> Integer
(define grid-y->pixel grid-x->pixel)

(define WORLD-SCENE 
  (freeze (for*/fold ([scn (empty-scene WIDTH HEIGHT)]) ([w (/ WIDTH CELL-SIZE)][h (/ HEIGHT CELL-SIZE)])
            (draw-cell w h scn))))