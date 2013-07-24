#lang racket

;; Goals and Directives Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (struct-out location)
         (struct-out move-toward)
         (struct-out hold-position)
         goal-achieved?)

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Goal is oneof:
;; Location

;; A Location is (location 2Vector Number)
(struct location (position) #:transparent)

;; A Directive is one of:
;; Move-Toward
;; Hold-Position

;; A Move-Toward is a (move-toward 2Vector)
(struct move-toward (position) #:transparent)

;; A Hold-Position is a (hold-position)
(struct hold-position () #:transparent)

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
  (= (send positionable position) (location-position lgoal)))

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