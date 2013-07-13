#lang racket
(require 2htdp/universe "world.rkt" "squad.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run)
  (big-bang (list (squaddie 50+50i 500+500i)
                  (squaddie 500+50i 100+150i))
            (to-draw draw-world)
            (on-tick tick)))


