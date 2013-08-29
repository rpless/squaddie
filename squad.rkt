#lang racket
(require 2htdp/image
         (only-in "data.rkt" within-width? within-height? positionable<%>)
         "utilities/2vector.rkt"
         "goals.rkt"
         "directives.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 define-squaddie
 squad%
 (contract-out 
  [squad/c contract?]
  #;[squad% squad/c])
 SPEED)

;; Data Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Squaddie is a (new squad% [pos 2Vector])
(define squad/c
  (recursive-contract
   (class/c
    ;; Draw the squaddie
    [draw (->m image? image?)]
    ;; Get the position of this squaddie
    [position (->m 2vector/c)]
    ;; Move this squaddie to the given location.
    [move (->dm ([v 2vector/c])
                #:pre (and (within-width? (2vector-x v))
                           (within-height? (2vector-y v)))
                (instanceof/c squad/c))]
    ;; Does this squaddie collide with the 
    [collide? (->m (is-a?/c positionable<%>) boolean?)])))

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define SPEED 5)
(define SIZE 5)
(define SQUADDIE-IMAGE (circle SIZE 'solid 'blue))

;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-squaddie <id> ((<method-name> <arg> ...) <code> ...)) -> class%
(define-syntax (define-squaddie stx)
  (syntax-case stx ()
    [(_ id ((method args ...) clauses ...))
     #`(define id 
         (class squad% (super-new) (inspect #f)
           (define/override (method args ...) 
             (begin clauses ...))))]))

;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define squad%
  (class* object% (positionable<%>)
    (init pos)
    (define in:pos pos)
    
    (abstract location-goal)
    
    (define/public-final (position) in:pos)
    
    (define/public-final (move vec) (new this% [pos vec]))
    
    (define/public-final (handle-goal goal)
      (send this execute-directive 
            (match goal
              [(location pos counter) (send this location-goal pos counter)])))
    
    (define/public-final (execute-directive directive)
      (match directive
        [(move-toward pos) (make-object this% (handle-directive directive this))]
        [(hold-position) (make-object this% (send this position))]))
    
    (define/public-final (collide? positionable)
      (< (distance (send this position) (send positionable position))
         SPEED))
        
    (define/public (draw scn)
      (let ([pos (send this position)])
        (place-image SQUADDIE-IMAGE (2vector-x pos) (2vector-y pos) scn)))
    
    (super-new)
    (inspect #f)))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  ;; Examples
  (define-squaddie test-squaddie% ((location-goal coord counter)
                                   (if (= coord (send this position))
                                       (hold-position)
                                       (move-toward coord))))
  (define squad1 (new test-squaddie% [pos 0+0i]))
  (define squad2 (new test-squaddie% [pos 10+10i]))
  
  ;; Movable Tests
  (check-equal? (send squad1 position) 0+0i)
  (check-equal? (send squad2 position) 10+10i)
  
  (check-equal? (send squad1 move 3+3i) (make-object test-squaddie% 3+3i))
  (check-equal? (send squad2 move 234+34i) (make-object test-squaddie% 234+34i))
  
  ;; Drawable Trait Tests
  (check-equal? (send squad2 draw (empty-scene 20 20))
                (place-image SQUADDIE-IMAGE 10 10 (empty-scene 20 20)))
  
  ;; Handle-goal Tests
  (check-equal? (send squad1 handle-goal (location 10+0i 1))
                (make-object test-squaddie% 5+0i))
  (check-equal? (send squad1 handle-goal (location 1+0i 1))
                (make-object test-squaddie% 1+0i))
  
  (check-equal? (send squad1 handle-goal (location 0+0i 1)) squad1)
  
  ;; collide? test
  (check-false (send squad1 collide? squad2))
  (check-true (send squad1 collide? squad1))
  (check-true (send squad1 collide? (new test-squaddie% [pos 0+4i]))))