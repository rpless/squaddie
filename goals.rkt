#lang racket

;; Goals Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide goal-position
         (struct-out location)
         goal-achieved?)

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Goal is oneof:
;; Location

(struct goal (position) #:transparent)

;; A Location is (location 2Vector)
(struct location goal () #:transparent)

;; Update Goals
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Goal Positionable -> Boolean
;; Has the given goal been achieved by the given Positionable?
(define (goal-achieved? goal positionable)
  (match goal
    [(location pos) (achieved-location-goal? goal positionable)]))

;; Location Positionable -> Boolean
;; Has the given Positionable reached the goal?
(define (achieved-location-goal? lgoal positionable)
  (= (send positionable position) (goal-position lgoal)))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test 
  (require rackunit)
  (define test-positionable% (class object%
                               (init pos) 
                               (define in:position pos) 
                               (define/public (position) in:position)
                               (super-new)))
  (let ()
    ;; Examples
    (define lgoal (location 0+0i))
    (define s1 (make-object test-positionable% 10+0i))
    (define s2 (make-object test-positionable% 0+0i))
    
    (check-true (goal-achieved? lgoal s2))
    (check-false (goal-achieved? lgoal s1))))