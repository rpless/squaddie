#lang racket
(require racket/trait 2htdp/image "data.rkt" "utilities/2vector.rkt")

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide
 define-squaddie
 (contract-out 
  [squad% squad/c])
 SPEED)

;; Data Definition
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Squaddie is a (new squad% [pos 2Vector])
(define squad/c
  (recursive-contract
   (class/c
    ;; Get the position of this squaddie
    [position (->m 2vector/c)]
    ;; Move this squaddie to the given location.
    [move (->m 2vector/c (instanceof/c squad/c))])))


;; A squaddie is a (squaddie Complex)
(struct squaddie (pos) #:transparent)

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define SPEED 5)
(define SQUADDIE-IMAGE (circle 5 'solid 'blue))

;; Syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-squaddie stx)
  (syntax-case stx (algorithm)
    [(_ id ((algorithm goal) clauses ...))
     #`(define id 
         ((trait->mixin (trait (define/public (handle-goal goal) clauses ...))) squad%))]))



;; Implementation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define squad%
  (class* object% (drawable<%>)
    (init pos)
    (define in:pos pos)
    
    (define/public (position) in:pos)
    
    (define/public (move vec) 
      (new this% [pos vec]))
    
    (define/public (draw scn)
      (let ([pos (send this position)])
      (place-image SQUADDIE-IMAGE (2vector-x pos) (2vector-y pos) scn)))
    
    (inspect #f)
    (super-new)))