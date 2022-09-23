#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; camera.rkt module
;; Alexis Elliott
;;
;; This module implements the Camera abstraction
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; include testing framework
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "math-util.rkt")
(require "color.rkt")

;; the representation of a Camera for the ray tracer.
(define-struct Camera
  [(wid : Natural)           ;; width of image
   (ht : Natural)            ;; height of image
   (n-samples : Natural)     ;; number of samples per pixel
   (origin : Float3)         ;; where the camera is located
   (ulc : Float3)            ;; upper-left-corner of image plane
   (h-vec : Float3)          ;; horizontal pixel-wide vector parallel to image
                             ;; pointing right
   (v-vec : Float3)])        ;; vertical pixel-wide vector parallel to image
                             ;; pointing down

(: simple-camera : Natural Natural Natural Float -> Camera)
;; make a camera that is equivalent to a Project 1 camera (for
;; testing purposes)
(define (simple-camera wid ht ns flen)
  (local
    {(define pw : Float (/ 2.0 (->fl wid)))}
    (Camera wid ht ns
            fl3-zero
            (Float3 -1.0
                    (/ (->fl ht) (->fl wid))
                    (- flen))
            (Float3 pw 0.0 0.0)
            (Float3 0.0 (- pw) 0.0))))

;; CODE FOR make-camera HERE

(: make-camera : Natural Natural Natural Float3 Float3 Float3 Float -> Camera)
;; make a camera.  The arguments are (in order):
;;   - width of image
;;   - height of image
;;   - number of samples per pixel
;;   - origin of camera in the world
;;   - point that the camera is looking at
;;   - up vector
;;   - horizontal field of view (in degrees)
(define (make-camera wid ht ns pos look-at up fov)
  (local
    {(define D : Float3 (fl3-normalize (fl3- look-at pos)))
     (define R : Float3 (fl3-normalize (fl3-cross D up)))
     (define U : Float3 (fl3-normalize (fl3-cross R D)))
     (define w : Float (->fl wid))
     (define pw : Float (/ 2.0 wid))
     (define H-vec :  Float3 (fl3-scale pw R))
     (define V-vec : Float3 (fl3-scale (- pw) U ))
     (define flen : Float (/ 1.0 (tan (degrees->radians (/ fov 2)))))
     (define Cimg : Float3 (fl3+ pos (fl3-scale flen D)))
     (define ULC : Float3 (fl3- (fl3+ Cimg (fl3-scale (/ (->fl ht) w) U)) R))}
    (Camera wid ht ns pos ULC H-vec V-vec)))


;; A Pixel-Renderer is a function that takes the row and column of a pixel
;; and produces a Racket Image-Library Color
(define-type Pixel-Renderer (Natural Natural -> Color))

(: foreach-pixel : Camera Pixel-Renderer -> Image)
;; given a camera and a pixel renderer, generate an image.
;;
(define (foreach-pixel cam pixel-renderer)
  (match cam
    [(Camera wid ht _ _ _ _ _)
     (if (or (= wid 0) (= ht 0))
         empty-image
         (local
           {(: for-rows : Natural (Listof Color) -> (Listof Color))
            ;; iterate over the rows of the image from bottom to top
            (define (for-rows row pixels)
              (if (= 0 row)
                  pixels
                  (for-cols (- row 1) wid pixels)))
            (: for-cols :  Natural Natural (Listof Color) -> (Listof Color))
            ;; iterate over the columns of a row from right to left
            (define (for-cols row col pixels)
              (if (= 0 col)
                  (for-rows row pixels)
                  (for-cols
                   row
                   (- col 1)
                   (cons (pixel-renderer row (- col 1)) pixels))))}
           (color-list->bitmap
            (for-rows ht '())
            wid ht)))]))

(: make-pixel-renderer : (Natural Natural -> RGB) (RGB -> Color) -> Pixel-Renderer)
;; compose a function that maps pixel coordinates to RGB values with
;; an RGB to Image-Library Color converter
(define (make-pixel-renderer pixel->rgb rgb->color)
  (lambda ([row : Natural] [col : Natural]) (rgb->color (pixel->rgb row col))))

;; CODE FOR ray-for-pixel GOES HERE

(: ray-for-pixel : Camera -> (Natural Natural -> Ray))
;; takes a camera and returns a function for generating a ray for a pixel
;; specified by its row and column
;; ray’s origin will be the camera’s position
;; ray’s direction is the vector from the camera’s position to the pixel's center 

(define (ray-for-pixel cam)
  (match cam
    [(Camera wid h n origin ulc h-vec v-vec)
     (local
       {(define up-l-center : Float3 (fl3+ ulc (fl3-scale 0.5 (fl3+ h-vec v-vec))))}
       (lambda ([row : Natural] [col : Natural])
         (local
           { (define C : Float3
               (fl3+ (fl3+ up-l-center (fl3-scale (->fl row) v-vec))
                     (fl3-scale (->fl col) h-vec))) }
           (make-ray origin (fl3- C origin)
                     ))))]))

;; CODE FOR rays-for-pixel GOES HERE

(: rays-for-pixel : Camera -> (Natural Natural -> (Listof Ray)))
;; given a camera, return a function that maps pixel coordinates in
;; the image plane to a list of rays from the camera through the pixel.
;; The number of rays is determined by the n-samples field of the Camera.
;; given a camera and the row and column of a pixel,
;; returns a list of random rays through that pixel.
(define (rays-for-pixel cam)
  (lambda ([row : Natural] [col : Natural])
    (match cam
      [(Camera wid ht n origin ulc h-vec v-vec)
       (build-list n (lambda ([ n : Index])
                       (make-ray origin
                                 (fl3- (fl3+ (fl3+ ulc (fl3-scale (+ row (random)) v-vec))
                                             (fl3-scale (+ col (random)) h-vec)) origin))
                       ))])))

;; CODE FOR pixel->rgb GOES HERE

(: pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces the ray for the given pixel
(define (pixel->rgb cam rt-fns)
  (lambda ([row : Natural] [col : Natural])
    (local
      {(define rfp : (Natural Natural -> Ray) (ray-for-pixel cam))}
      (rt-fns (rfp row col)))))

;; CODE FOR antialias-pixel->rgb GOES HERE

(: antialias-pixel->rgb : Camera (Ray -> RGB) -> Natural Natural -> RGB)
;; given a camera and a ray-tracing function, return a function that
;; traces a list of rays for the given pixel and returns their average
;; sum the RGB values from tracing the individual rays for the pixel and
;; then scale the sum by 1/n-samples (if n is 0, scale by 1)
(define (antialias-pixel->rgb cam rt-fns)
  (lambda ([row : Natural] [col : Natural])
    (local
      {(define rsfp : (Natural Natural -> (Listof Ray)) (rays-for-pixel cam))
       (define s : Float (match cam
                           [(Camera _ _ n _ _ _ _) (if (= n 0) 1.0 (/ 1.0 n))]))}
      (rgb-scale s (foldl (lambda ([r : Ray] [rgb-sum : RGB]) (rgb+ (rt-fns r) rgb-sum))
             (RGB 0.0 0.0 0.0)
             (rsfp row col))))))

(: ray->rgb : Ray -> RGB)
;; a function for testing ray generation.  It maps a ray to a color in
;; the white-to-blue range based on the Y component of the ray's direction
;; vector.
(define (ray->rgb ray)
  (match ray
    [(Ray _ dir)
     (local
       {(define t : Float (* 0.5 (+ 1.0 (Float3-y dir))))}
       (rgb-lerp rgb-white t (RGB 0.5 0.7 1.0)))]))

(test)

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide Camera)

(provide make-camera
         simple-camera
         foreach-pixel
         make-pixel-renderer
         pixel->rgb
         antialias-pixel->rgb
         ray->rgb)
