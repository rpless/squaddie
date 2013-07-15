#lang racket
(require (only-in 2htdp/image image?))

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide WIDTH HEIGHT CELL-SIZE drawable<%> in-width/c in-height/c)

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define WIDTH 800)
(define HEIGHT 800)
(define CELL-SIZE 50)

(define drawable<%> 
  (interface ()
    [draw (->m image? image?)]))

;; Contracts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define in-width/c (real-in 0 WIDTH))
(define in-height/c (real-in 0 HEIGHT))