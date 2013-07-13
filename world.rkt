#lang racket
(require 2htdp/image "map.rkt" "squad.rkt" "data.rkt")

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

;; Tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> SquadWorld
(define (tick sworld) 
  (cond [(achieve-current-goal? sworld) 
         (world (world-squad sworld) (rest (world-goals sworld)))]
        [else (world (tick-squaddie (world-squad sworld) (first (world-goals sworld)))
                     (world-goals sworld))]))

;; SquadWorld -> Boolean
(define (achieve-current-goal? sworld)
  (let ([spos (squaddie-pos (world-squad sworld))])
    (= spos (first (world-goals sworld)))))


;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> Scene
(define (draw-world sworld)
  (foldr draw-goals (draw-squaddie (world-squad sworld) WORLD-SCENE) (world-goals sworld)))

(define (draw-end-game sworld)
  (place-image (text "Goals Achieved" 36 'black)
               (/ WIDTH 2)
               (/ HEIGHT 2)
               (draw-world sworld)))

(define (draw-goals goal scn)
  (place-image (triangle 20 'outline 'green)
               (real-part goal)
               (imag-part goal)
               scn))

;; End Game
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> Boolean
(define (game-over? sworld)
  (empty? (world-goals sworld)))