#lang racket
(require 2htdp/image
         2htdp/universe
         (only-in "data.rkt"  WIDTH HEIGHT)
         "utilities/2vector.rkt"
         "goals.rkt")

;; World Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The world module provides a representation of the world.

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

;; A SquadWorld is a (new world% [squad Squaddie] [goal [Listof Goal]])
(define world%
  (class object%
    (init-field squad goals)
    
    (define/public-final (tick)
      (cond [(empty? goals) (make-object end-world% squad)]
            [(goal-achieved? (first goals) squad) (make-object this% squad (rest goals))]
            [else (update)]))
    
    ;; -> SquadWorld
    ;; update the squad
    (define/private (update) 
      (make-object this% (send squad handle-goal (first goals)) goals))
    
    (define/public-final (game-over?) #f)
    
    (define/public (draw scn)
      (draw-goals (send squad draw scn)))
    
    (define/private (draw-goals scn)
      (foldr (λ (g s) (draw-goal g s)) scn goals))
    
    ;; Goal Scene -> Scene
    ;; Draw the goal on the screen
    (define/private (draw-goal goal scn)
      (let ([g (goal-position goal)])
        (place-image GOAL-IMAGE (2vector-x g) (2vector-y g) scn)))
    
    (super-new)
    (inspect #f)))

;; A EndWorld is a (new end-world% [squad Squaddie])
(define end-world%
  (class object%
    (init-field squad)
    (define/public-final (tick) this)
    
    (define/public-final (game-over?) #t)
    
    (define/public (draw scn)
      (place-image GAME-OVER-TEXT (/ WIDTH 2) (/ HEIGHT 2) (send squad draw scn)))
    
    (super-new)
    (inspect #f)))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit "squad.rkt" "goals.rkt" "directives.rkt")
  (define-squaddie test-squaddie% ((location-goal coord) (move-toward coord))))

;; World Tests
(module+ test 
  (let ()
    ;; Examples
    (define squad1 (new test-squaddie% [pos 0+0i]))
    (define squad2 (new test-squaddie% [pos 10+0i]))
    
    (define goal (location 10+0i))
    (define w1 (make-object world% squad1 (list goal)))
    (define w2 (make-object world% squad2 (list goal)))
    (define end (make-object end-world% squad1))
    
    ;; Test draw
    (check-equal? (send w2 draw (empty-scene 500 500))
                  (place-image GOAL-IMAGE 10 0 (send squad2 draw (empty-scene 500 500))))
    (check-equal? (send end draw (empty-scene 500 500))
                  (place-image GAME-OVER-TEXT (/ WIDTH 2) (/ HEIGHT 2) (send squad1 draw (empty-scene 500 500))))
    
    ;; Test Tick
    (check-equal? (send end tick) end)
    (check-equal? (send w1 tick) (make-object world% (send squad1 handle-goal goal) (list goal)))
    (check-equal? (send w2 tick) (make-object world% squad2 '()))
    (check-equal? (send (make-object world% squad1 '()) tick) (make-object end-world% squad1))
    
    ;; Test game-over?
    (check-true (send end game-over?))
    (check-false (send w1 game-over?))))