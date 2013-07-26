#lang racket
(require 2htdp/image "utilities/2vector.rkt")

;; Goals Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide goal-position
         (struct-out location)
         goal-achieved?
         draw-goal
         update-goal)

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Goal is oneof:
;; Location

(struct goal (position) #:transparent)

;; A Location is (location 2Vector Integer)
(struct location goal (counter) #:transparent)

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define GOAL-IMAGE (overlay (circle 1 'solid 'red) (triangle 20 'outline 'green)))

;; Update Goals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Goal -> Boolean
;; Has the given goal been achieved?
(define (goal-achieved? goal)
  (match goal
    [(location pos count) (achieved-location-goal? goal)]))

;; Location -> Boolean
;; Has the location been held?
(define (achieved-location-goal? lgoal)
  (<= (location-counter lgoal) 0))

;; Goal -> Goal
(define (update-goal goal positionable)
  (match goal
    [(location pos count) (update-location goal positionable)]))

;; Location Positionable -> Location
(define (update-location lgoal positionable)
  (let ([pos (goal-position lgoal)])
    (if (= (send positionable position) pos)
        (location pos (sub1 (location-counter lgoal)))
        lgoal)))

;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (draw-goal goal scn)
  (let ([g (goal-position goal)])
    (place-image GOAL-IMAGE (2vector-x g) (2vector-y g) scn)))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test 
  (require rackunit)
  (define test-positionable% (class object%
                               (init pos) 
                               (define in:position pos) 
                               (define/public (position) in:position)
                               (super-new)))
  ;; goal-achieved?
  (let ()
    ;; Examples
    (define g1 (location 0+0i 1))
    (define g2 (location 0+0i 0))
    
    (check-false (goal-achieved? g1))
    (check-true (goal-achieved? g2)))
  
  ;; update-goal
  (let ()
    (define g1 (location 0+0i 1))
    (define s1 (make-object test-positionable% 0+0i))
    (define s2 (make-object test-positionable% 10+0i))
    
    (check-equal? (update-goal g1 s1) (location 0+0i 0))
    (check-equal? (update-goal g1 s2) g1))
  
  ;; draw-goal
  (let ()
    (check-equal? (draw-goal (location 9+9i 1) (empty-scene 20 20))
                  (place-image GOAL-IMAGE 9 9 (empty-scene 20 20)))))