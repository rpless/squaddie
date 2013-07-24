#lang racket
(require (only-in 2htdp/image image?))

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide WIDTH HEIGHT CELL-SIZE
         in-width/c in-height/c
         within-width? within-height?
         positionable<%>)

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Positionable is an object that has a position
(define positionable<%> 
  (interface ()
    ;; -> 2Vector
    position))

;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define WIDTH 800)
(define HEIGHT 600)
(define CELL-SIZE 50)

;; Contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define in-width/c (real-in 0 WIDTH))
(define in-height/c (real-in 0 HEIGHT))

(define (within-width? x)
  (<= 0 x WIDTH))

(define (within-height? y)
  (<= 0 y HEIGHT))