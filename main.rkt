#lang racket
(require "config.rkt"
         "squad.rkt" 
         "utilities/2vector.rkt"
         "directives.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; An Example Squaddie
(define-squaddie my-squaddie% 
  ((location-goal loc count) 
   (let ([pos (send this position)])
     (cond [(= pos loc) (hold-position)]
           [else (move-toward loc)]))))

(define (run)
  (run-game my-squaddie%))
