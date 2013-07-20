#lang racket

;; Goals and Directives Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(provide (struct-out location)
         (struct-out move-toward))

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A Goal is oneof:
;; Location

;; A Location is (location 2Vector)
(struct location (position) #:transparent)

;; A Directive is one of:
;; - Move-Toward

;; A Move-Toward is a (move-toward 2Vector)
(struct move-toward (position) #:transparent)