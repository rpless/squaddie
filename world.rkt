#lang racket
(require 2htdp/image
         2htdp/universe
         (only-in "data.rkt"  WIDTH HEIGHT)
         "utilities/2vector.rkt"
         "goals.rkt")

;; World Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The world module provides a representation of the world.

;; REFACTOR:
;; -  Make a config object that has the map and specifies a starting location
;; -  make the world an abstract that can tick the squads. mixin drawing behavior

(provide 
 world%
 big-bang-with-class
 (contract-out 
  [world/c contract?]
  #;[world% world/c]))

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SquadWorld is a (world squad [Listof Goal])
(struct world (squad goals) #:transparent)

(define world/c
  (recursive-contract
   (class/c
    ;; Progress to the next world state
    [tick (->m (instanceof/c world/c))]
    ;; Is the game over?
    [game-over? (->m boolean?)]
    ;; Draw the world
    [draw (->m image? image?)])))


;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define GOAL-IMAGE (overlay (circle 1 'solid 'red) (triangle 20 'outline 'green)))
(define GAME-OVER-TEXT (text "Goals Achieved" 36 'black))

;; Syntactic Sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (big-bang-with-class obj base-scene)
  (big-bang obj
            [to-draw   (λ (o) (send o draw base-scene))]
            [on-tick   (λ (o) (send o tick))]
            [stop-when (λ (o) (send o game-over?)) (λ (o) (send o draw base-scene))]))

;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A SquadWorld is a (new world% [squad Squaddie] [goal Goal])
(define world%
  (class object%
    (init-field squad goal)
    
    (define/public (tick)
      (if (goal-achieved?)
          (make-object end-world% squad)
          (update)))
    
    ;; -> SquadWorld
    ;; update the squad
    (define/private (update) 
      (make-object this% (send squad handle-goal goal) goal))
    
    ;; -> Boolean
    ;; has the squaddie reached its goal
    (define/private (goal-achieved?)
      (let ([spos (send squad position)])
        (= spos (location-position goal))))
    
    (define/public (game-over?) #f)
    
    (define/public (draw scn)
      (draw-goal (send squad draw scn)))
    
    ;; Scene -> Scene
    ;; Draw the goal on the screen
    (define/private (draw-goal scn)
      (let ([g (location-position goal)])
        (place-image GOAL-IMAGE (2vector-x g) (2vector-y g) scn)))
    
    (super-new)
    (inspect #f)))

;; A EndWorld is a (new end-world% [squad Squaddie])
(define end-world%
  (class object%
    (init-field squad)
    (define/public (tick) this)
    
    (define/public (game-over?) #t)
    
    (define/public (draw scn)
      (place-image GAME-OVER-TEXT 
                   (/ WIDTH 2) 
                   (/ HEIGHT 2)
                   (send squad draw scn)))
    (super-new)
    (inspect #f)))
