#lang racket
(require "utilities/2vector.rkt")

;; Directives Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (struct-out move-toward)
         (struct-out hold-position)
         handle-directive)

;; Contants
(define SPEED 5)

;; A Directive is one of:
;; Move-Toward
;; Hold-Position

;; A Move-Toward is a (move-toward 2Vector)
(struct move-toward (position) #:transparent)

;; A Hold-Position is a (hold-position)
(struct hold-position () #:transparent)

;; Update Directives
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; 2Vector -> 2Vector
(define (handle-directive directive positionable)
  (match directive
    [(move-toward pos) (handle-move-directive pos positionable)]))

;; 2Vector -> 2Vector
(define (handle-move-directive goal positionable)
  (let ([pos (send positionable position)])
    (if (<= (distance pos goal) SPEED)
        goal
        (+ pos (* SPEED (normalize (- goal pos)))))))