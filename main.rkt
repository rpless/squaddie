#lang racket
(require 2htdp/universe "map.rkt" "squad.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run)
  (big-bang (list (squaddie 50+50i 500+500i)
                  (squaddie 500+50i 100+150i))
            (to-draw render-world)
            (on-tick tick)))

;; Tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> SquadWorld
(define (tick sworld) (map tick-squaddie sworld))

;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; SquadWorld -> Scene
(define (render-world sworld)
  (foldr draw-squaddie WORLD-SCENE sworld))
