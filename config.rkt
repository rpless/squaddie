#lang racket
(require (only-in "data.rkt" WIDTH HEIGHT CELL-SIZE)
         "world.rkt"
         "goals.rkt"
         2htdp/image)

;; Config Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; This module contains the Configuration for a game of Squaddie.
;; The configuration is specfied by the game's creator and then closed inside
;; a function that is given to the player to start a game.
;; It includes the background image and the starting point.

(provide run-game)

;; Run the Game.
(define goals (list (location 200+345i 1) (location 10+500i 10)))
(define (run-game %) (((create-game WORLD-SCENE) 50+50i goals) %))

;; Scene -> [2Vector [Listof Goal] -> [Squad -> Void]]
;; Create a game with the given background scene, starting position, goal, and squaddie.
(define (((create-game scene) start goals) %)
  (big-bang-with-class (make-object world% (make-object % start) goals) scene))

;; Drawing the World
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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