#lang racket
(require 2htdp/image 2htdp/universe)

;; Squaddie
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Constants
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define WIDTH 16)
(define HEIGHT 16)
(define CELL-SIZE 50)

(define WIDTH-PX (* CELL-SIZE WIDTH))
(define HEIGHT-PX (* CELL-SIZE HEIGHT))

;; Main
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (run)
  (big-bang '()
            (to-draw render-world)))

;; Tick
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Rendering
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; World -> Scene
(define (render-world sworld)
  (for*/fold ([scn (empty-scene WIDTH-PX HEIGHT-PX)]) ([w WIDTH][h HEIGHT])
    (draw-cell w h scn)))

;; Integer Integer Scene -> Scene
(define (draw-cell width height scn)
  (place-image (overlay (circle 1 'solid 'red)
                        (rectangle CELL-SIZE CELL-SIZE 'outline 'black))
               (coord-x->pixel width)
               (coord-y->pixel height)
               scn))

;; Integer -> Integer
(define (coord-x->pixel x)
  (+ (* CELL-SIZE x) (/ CELL-SIZE 2)))

;; Integer -> Integer
(define coord-y->pixel coord-x->pixel)
