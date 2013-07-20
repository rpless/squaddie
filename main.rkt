#lang racket
(require "config.rkt"
         "squad.rkt" 
         "utilities/2vector.rkt" 
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
(define (run)
  (run-game my-squaddie%))
