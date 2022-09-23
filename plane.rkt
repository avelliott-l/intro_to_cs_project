#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; plane.rkt module
;; Alexis Elliott
;;
;; This module contains the implementation of the plane object
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

;; CODE FOR make-plane GOES HERE

(: make-plane : Float3 Float3 Material -> Object)
;; (make-plane pt perp material) makes a plane object.  The plane
;; contains the point pt and its orientation is defined by the
;; vector perp, which is perpendicular to the plane.  The third
;; argument specifies the plane's surface material.
;; Note that the perpendicular vector does not have to be unit length.
(define (make-plane pt perp mat)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (local
       {(define top : Float (fl3-dot (fl3- pt (Ray-origin ray)) perp))
        (define bot : Float (fl3-dot (Ray-dir ray) perp))
        (define t : Float (/ top bot))}
       (cond
         [(= bot 0) miss] ;; ray doesn't intersect plane
         [(<= min-t t)
          (if (< top 0) (list
                         (Hit 'IN t (ray-point-at ray t) ;; ray starts outside half space
                              (fl3-normalize perp)
                              mat)
                         (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black)
                         )
              (list
               (Hit 'OUT t (ray-point-at ray t)  ;; ray starts inside plane
                    (fl3-normalize perp)         ;; single exit hit
                    mat)))]
         [(< bot 0) (list (Hit 'OUT +inf.0 fl3-zero fl3-zero flat-black))]
         [else miss]))))) ;; no hit

(test)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide make-plane)
