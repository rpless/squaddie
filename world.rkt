#lang racket
(require 2htdp/image "map.rkt" "squad.rkt" "data.rkt" "utilities/2vector.rkt")

;; World Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The world module provides a representation of the world.

(provide (struct-out world) tick draw-world game-over? draw-end-game)

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SquadWorld is a (world squad [Listof Goal])
(struct world (squad goals) #:transparent)

;; A Goal is oneof:
;; Location

;; A Location is a Complex

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define GOAL-IMAGE (overlay (circle 1 'solid 'red) (triangle 20 'outline 'green)))
(define GAME-OVER-TEXT (text "Goals Achieved" 36 'black))

;; Tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> SquadWorld
(define (tick sworld) 
  (cond [(achieve-current-goal? sworld) 
         (world (world-squad sworld) (rest (world-goals sworld)))]
        [else (world (send (world-squad sworld) handle-goal (first (world-goals sworld)))
                     (world-goals sworld))]))

;; SquadWorld -> Boolean
(define (achieve-current-goal? sworld)
  (let ([spos (send (world-squad sworld) position)])
    (= spos (first (world-goals sworld)))))


;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> Scene
(define (draw-world sworld)
  (foldr draw-goals 
         (send (world-squad sworld) draw WORLD-SCENE) 
         (world-goals sworld)))

(define (draw-goals goal scn)
  (place-image GOAL-IMAGE (2vector-x goal) (2vector-y goal) scn))

;; End Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> Boolean
(define (game-over? sworld)
  (empty? (world-goals sworld)))

;; SquadWorld -> Image
(define (draw-end-game sworld)
  (place-image GAME-OVER-TEXT (/ WIDTH 2) (/ HEIGHT 2) (draw-world sworld)))