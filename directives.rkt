#lang racket

;; Directives Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (struct-out move-toward)
         (struct-out hold-position))

;; A Directive is one of:
;; Move-Toward
;; Hold-Position

;; A Move-Toward is a (move-toward 2Vector)
(struct move-toward (position) #:transparent)

;; A Hold-Position is a (hold-position)
(struct hold-position () #:transparent)