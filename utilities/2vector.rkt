#lang racket

;; 2Vector Module
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; The 2Vector module defines the 2Vector data type and operations that can
;; performed on it.

(provide 
 (contract-out 
  ;; contract for a 2vector
  [2vector/c contract?]
  ;; Get the vector's x component
  [2vector-x (-> 2vector/c real?)]
  ;; Get the vector's y component
  [2vector-y (-> 2vector/c real?)]
  ;; Get the distance between two 2vectors
  [distance (-> 2vector/c 2vector/c real?)]))

;; Data
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; A 2Vector is a Complex
(define 2vector/c complex?)

;; Operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; All of the mathematical operations that can be done on complex numbers can
;; performed on 2Vectors.

(define 2vector-x real-part)

(define 2vector-y imag-part)

(define (distance c1 c2)
  (sqrt (+ (sqr (- (real-part c2) (real-part c1)))
           (sqr (- (imag-part c2) (imag-part c1))))))


;; Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(module+ test
  (require rackunit)
  
  ;; Distance Tests
  (check-equal? (distance 0+0i 1+0i) 1)
  (check-equal? (distance 0+0i 0+1i) 1)
  (check-equal? (distance 3+0i 0+4i) 5))