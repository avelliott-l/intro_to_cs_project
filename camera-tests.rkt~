#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; camera-tests.rkt module
;;
;; Eyeball tests for the camera.rkt module.
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
(require "color.rkt")
(require "camera.rkt")

(define cam-400x200x1-0.0 (simple-camera 400 200 1 0.0))
(define cam-400x200x1-0.25 (simple-camera 400 200 1 0.25))
(define cam-400x200x1 (simple-camera 400 200 1 1.0))
(define cam-400x200x4 (simple-camera 400 200 4 0.25))

;; Test foreach-pixel
;;
"== eyeball test foreach-pixel"
(local
  {(define wid : Natural 400)
   (define ht : Natural 200)}
  (foreach-pixel
   cam-400x200x1-0.0
   (make-pixel-renderer
    (lambda ([r : Natural] [c : Natural])
      (if (and (< r ht) (< c wid))
          (RGB (/ (->fl c) (->fl wid))
               (/ (->fl (- ht r)) (->fl ht))
               0.2)
      (error "row/column out of range")))
   rgb->color)))

"== background test"
(foreach-pixel cam-400x200x1-0.25
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1-0.25 ray->rgb)
                rgb->color))

"== test ray-for-pixel"
(foreach-pixel cam-400x200x1-0.25
               (make-pixel-renderer
                (pixel->rgb cam-400x200x1-0.25
                            (lambda ([ray : Ray])
                              (match ray
                                [(Ray _ dir)
                                 (RGB
                                  (* 0.5 (+ 1.0 (Float3-x dir)))
                                  (* 0.5 (+ 1.0 (Float3-y dir)))
                                  (* 0.5 (+ 1.0 (Float3-z dir))))])))
                rgb->color))

"== AA-4 background"
(foreach-pixel cam-400x200x4
               (make-pixel-renderer
                (antialias-pixel->rgb cam-400x200x4 ray->rgb)
                rgb->color))
