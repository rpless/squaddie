#lang racket
(require 2htdp/universe "world.rkt" "squad.rkt" "utilities/2vector.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-squaddie my-squaddie% 
  ((algorithm goal) 
   (let ([pos (send this position)])
     (make-object this% 
       (if (<= (distance pos goal) SPEED)
           goal
           (+ pos (* SPEED (normalize (- goal pos)))))))))

(define (run)
  (big-bang (world (make-object my-squaddie% 50+50i) '(500+500i 200+345i))
            (to-draw draw-world)
            (on-tick tick)
            (stop-when game-over? draw-end-game)))