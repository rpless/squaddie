#lang racket
(require 2htdp/universe "world.rkt" "squad.rkt" "utilities/2vector.rkt" "config.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Syntactic Sugar
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (big-bang-with-class obj)
    (big-bang obj
              [to-draw (λ (o) (send o draw WORLD-SCENE))]
              [on-tick (λ (o) (send o tick))]
              [stop-when (λ (o) (send o game-over?)) (λ (o) (send o draw WORLD-SCENE))]))

;; REFACTOR
;; -  Create a curried function that first takes a config and then a world
;;    The game will be run by passing a squad(s) into this function

(define-squaddie my-squaddie% 
  ((algorithm goal) 
   (let ([pos (send this position)])
     (make-object this% 
       (if (<= (distance pos goal) SPEED)
           goal
           (+ pos (* SPEED (normalize (- goal pos)))))))))


(define (run)
  (big-bang-with-class (make-object world% (make-object my-squaddie% 50+50i) 200+345i)))