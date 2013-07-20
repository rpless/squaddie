#lang racket
(require racket/trait 2htdp/image "data.rkt" "utilities/2vector.rkt")

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

;; (define-squaddie <id> ((algorithm <id>) <code> ...)) -> class%
(define-syntax (define-squaddie stx)
  (syntax-case stx (algorithm)
    [(_ id ((algorithm goal) clauses ...))
     #`(define id 
         ((trait->mixin (trait (define/public (handle-goal goal) clauses ...))) squad%))]))

;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define squad<%>
  (interface () draw position move))

(define squad%
  (class* object% (squad<%>)
    (init pos)
    (define in:pos pos)
    
    (define/public (position) in:pos)
    
    (define/public (move vec) (new this% [pos vec]))
    
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
  (define squad1 (new squad% [pos 0+0i]))
  (define squad2 (new squad% [pos 10+10i]))
  
  ;; Movable Tests
  (check-equal? (send squad1 position) 0+0i)
  (check-equal? (send squad2 position) 10+10i)
  
  (check-equal? (send squad1 move 3+3i) (make-object squad% 3+3i))
  (check-equal? (send squad2 move 234+34i) (make-object squad% 234+34i))
  
  ;; Drawable Trait Tests
  (check-equal? (send squad2 draw (empty-scene 20 20))
                (place-image SQUADDIE-IMAGE 10 10 (empty-scene 20 20))))