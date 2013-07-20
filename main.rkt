#lang racket
(require 2htdp/universe 
         "world.rkt" 
         "squad.rkt" 
         "utilities/2vector.rkt" 
         "config.rkt" 
         "goals.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Example Squaddie
(define-squaddie my-squaddie% 
  ((location-goal goal) 
   (let ([pos (send this position)])
     (if (not (zero? (- (2vector-x pos) (2vector-x goal))))
         (move-toward (make-rectangular (2vector-x goal) (2vector-y pos)))
         (move-toward goal)))))

;; Scene -> [2Vector Goal -> [Squad -> Void]]
;; Create a game with the given background scene, starting position, goal, and squaddie.
(define (((create-game scene) start goal) %)
  (big-bang-with-class (make-object world% (make-object % start) goal) scene))

;; Run the Game.
(((create-game WORLD-SCENE) 50+50i (location 200+345i)) my-squaddie%)