#lang racket
(require 2htdp/universe "world.rkt" "squad.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run)
  (big-bang (world (squaddie 50+50i) '(500+500i 200+345i))
            (to-draw draw-world)
            (on-tick tick)
            (stop-when game-over? draw-end-game)))