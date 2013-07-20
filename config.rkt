#lang racket
(require (only-in "data.rkt" WIDTH HEIGHT CELL-SIZE)
         2htdp/image)
(provide WORLD-SCENE)

;; Config Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module contains the Configuration for a game of Squaddie.
;; The configuration is specfied by the game's creator and then closed inside
;; a function that is given to the player to start a game.
;; It includes the background image and the starting point.

;; Integer Integer Scene -> Scene
(define (draw-cell width height scn)
  (place-image (rectangle CELL-SIZE CELL-SIZE 'outline 'purple)
               (grid-x->pixel width)
               (grid-y->pixel height)
               scn))

;; Integer -> Integer
(define (grid-x->pixel x)
  (+ (* CELL-SIZE x) (/ CELL-SIZE 2)))

;; Integer -> Integer
(define grid-y->pixel grid-x->pixel)

(define WORLD-SCENE 
  (let ([base (rectangle (add1 WIDTH) (add1 HEIGHT) 'solid 'white)])
    (freeze (for*/fold ([scn base]) ([w (/ WIDTH CELL-SIZE)]
                                     [h (/ HEIGHT CELL-SIZE)])
              (draw-cell w h scn)))))