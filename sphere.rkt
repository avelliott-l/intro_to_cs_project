#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; sphere.rkt module
;;
;; This module contains the implementation of the sphere object
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "material.rkt")
(require "hit.rkt")
(require "object.rkt")

(: make-sphere : Float3 Float Material -> Object)
;; make a sphere object with the given center, radius, and material
(define (make-sphere center radius material)
  (local
    {(define radius^2 : Float (* radius radius))
     (: hit-test : Ray Float -> Hit-List)
     ;; (intersect R min-t) tests for an intersection between the ray R
     ;; and the sphere.  It returns a Hit record of the intersection; otherwise
     ;; it returns 'Miss if there is no intersection.
     (define (hit-test ray min-t)
       (local
         {(define ro : Float3 (Ray-origin ray))
          (define rd : Float3 (Ray-dir ray))
          (define q : Float3 (fl3- ro center))
          (define b : Float (* 2.0 (fl3-dot rd q)))
          (define c : Float (- (fl3-dot q q) radius^2))
          (define disc (- (* b b) (* 4.0 c)))}
         (if (<= disc 0.0)
             miss
             (local
               {(define sqrt-disc : Float (fl-sqrt disc))
                (define t0 : Float (* 0.5 (- (- b) sqrt-disc)))
                (define t1 : Float (* 0.5 (+ (- b) sqrt-disc)))
                (: make-hit : Parity Float -> Hit)
                ;; make a Hit value for the given parity and t value
                (define (make-hit p t)
                  (local
                    {(define pt : Float3 (ray-point-at ray t))}
                    (Hit p t pt (fl3-scale (/ 1.0 radius) (fl3- pt center)) material)))}
               (cond
                 [(< t1 min-t) miss]
                 [(< t0 min-t) (list (make-hit 'OUT t1))]
                 [else (list (make-hit 'IN t0) (make-hit 'OUT t1))])))))}
    (Object hit-test)))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-sphere)
