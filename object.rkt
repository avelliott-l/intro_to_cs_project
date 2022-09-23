#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; object.rkt module
;;
;; This module contains the definition of the graphical objects in the scene
;;

;; load custom definitions
;;
(require "../include/cs151-core.rkt")

;; load image library definitions
;;
(require "../include/cs151-image.rkt")

;; include the Flonum bindings for sqrt, etc.
;;
(require typed/racket/flonum)

;; load testing infrastructure
;;
(require typed/test-engine/racket-tests)

;; project modules
;;
(require "util.rkt")
(require "math-util.rkt")
(require "color.rkt")
(require "material.rkt")
(require "hit.rkt")

;;;;; Objects

;; An (Object hit-test) represents a collection of geometric objects as
;; a hit-test function
;;
(define-struct Object
  ([hit-test : (Ray Float -> Hit-List)]))    ;; hit test for object

(: hit-test : Object Ray Float -> Hit-List)
;; invoke the hit-test function of the object on the given ray
(define (hit-test obj ray min-t)
  ((Object-hit-test obj) ray min-t))

;; an empty object cannot be hit by rays
(define empty-object : Object
  (Object
   (lambda ([ray : Ray] [min-t : Float]) miss)))

(: merge-hit-lists : Hit-List Hit-List -> Hit-List)
;; given two valid hit lists, merge them into a valid hit list
(define (merge-hit-lists hits1 hits2)
  (match* (hits1 hits2)
    [('() _) hits2]
    [(_ '()) hits1]
    [((cons hit1 r1) (cons hit2 r2))
     (local
       {(: merge-hit : Hit Hit-List Parity Hit-List -> Hit-List)
        ;; helper function that merges the nearer hit into the list.  The
        ;; parameters are:
        ;;   hit1   -- the nearest hit in the two lists being merged
        ;;   r1     -- the rest of the hit list with the nearest hit
        ;;   p      -- the parity of the first hit in the other hit list
        ;;   hits2  -- the other hit list
        (define (merge-hit hit1 r1 p hits2)
          (match* ((Hit-parity hit1) p)
            [('OUT 'OUT) (merge-hit-lists r1 hits2)]
            [('OUT 'IN) (cons hit1 (merge-hit-lists r1 hits2))]
            [('IN 'OUT) (merge-hit-lists r1 hits2)]
            [('IN 'IN) (cons hit1 (merge-hit-lists r1 hits2))]))}
       (if (hit< hit1 hit2)
           (merge-hit hit1 r1 (Hit-parity hit2) hits2)
           (merge-hit hit2 r2 (Hit-parity hit1) hits1)))]))

(: list->object : (Listof Object) -> Object)
;; make an object from a list of objects
(define (list->object objs)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (local
       {(: look-for-hits : Object Hit-List -> Hit-List)
        ;; test the ray against the object and merge any resulting hits into the hit list
        (define (look-for-hits obj hits)
          (merge-hit-lists (hit-test obj ray min-t) hits))}
       (foldl look-for-hits miss objs)))))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide (struct-out Object))

(provide hit-test
         empty-object
         list->object)
