#lang racket
(require racket/trait
         2htdp/image
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
                (instanceof/c squad/c))])))

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define SPEED 5)
(define SQUADDIE-IMAGE (circle 5 'solid 'blue))

;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (define-squaddie <id> ((<method-name> <id>) <code> ...)) -> class%
(define-syntax (define-squaddie stx)
  (syntax-case stx ()
    [(_ id ((method goal) clauses ...))
     #`(define id 
         (class squad% (super-new) (inspect #f)
           (define/override (method goal) 
             (let ([res (begin clauses ...)])
               (send this handle-directive res)))))]))

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
      (match goal
        [(location pos) (send this location-goal pos)]))
    
    (define/public-final (handle-directive directive)
      (match directive
        [(move-toward pos) (make-object this% (handle-move-directive pos))]))
        
    (define/public (draw scn)
      (let ([pos (send this position)])
        (place-image SQUADDIE-IMAGE (2vector-x pos) (2vector-y pos) scn)))
    
    ;; 2Vector -> 2Vector
    (define/private (handle-move-directive goal)
      (let ([pos (send this position)])
        (if (<= (distance pos goal) SPEED)
            goal
            (+ pos (* SPEED (normalize (- goal pos)))))))
    
    (super-new)
    (inspect #f)))

;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test
  (require rackunit)
  
  ;; Examples
  (define-squaddie test-squaddie% ((location-goal coord) (move-toward coord)))
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
  (check-equal? (send squad1 handle-goal (location 10+0i))
                (make-object test-squaddie% 5+0i))
  (check-equal? (send squad1 handle-goal (location 1+0i))
                (make-object test-squaddie% 1+0i)))