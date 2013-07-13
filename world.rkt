#lang racket
(require "map.rkt" "squad.rkt")

(provide tick draw-world)

;; Tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> SquadWorld
(define (tick sworld) (map tick-squaddie sworld))

;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> Scene
(define (draw-world sworld) (foldr draw-squaddie WORLD-SCENE sworld))