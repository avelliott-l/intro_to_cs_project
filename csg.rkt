#lang typed/racket

;; CMSC 15100 Winter 2022
;; Project 2
;; csg.rkt module
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
(require "hit.rkt")
(require "object.rkt")
(require "material.rkt")


(: union-hit-lists : Hit-List Hit-List -> Hit-List)
;; given two hit lists corresponding to two objects, compute the hit list for
;; the union of the two objects.  We discard hits that do not change the status
;; of the ray w.r.t. the combined object.  For example, if we are inside the second
;; object, then an entry hit for the first object can be discarded.  We determine
;; the status of the ray based on the parity of the next hit in the list for the
;; object.
(define (union-hit-lists hits1 hits2)
  (match* (hits1 hits2)
    [('() _) hits2]
    [(_ '()) hits1]
    [((cons h1 r1) (cons h2 r2))
     (if (hit< h1 h2) 
         (local ;; 1st hit is closer
           {(define hits : Hit-List (union-hit-lists r1 hits2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) (cons h1 hits)]
             [('IN 'OUT) hits]
             [('OUT 'IN) (cons h1 hits)]
             [('OUT 'OUT) hits]))
         (local ;; second hit is closer
           {(define hits : Hit-List (union-hit-lists hits1 r2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) (cons h2 hits)]
             [('IN 'OUT) (cons h2 hits)]
             [('OUT 'IN) hits]
             [('OUT 'OUT) hits])))]))

(: object-union : Object Object -> Object)
;; return the union of two objects
(define (object-union obj1 obj2)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (union-hit-lists (hit-test obj1 ray min-t) (hit-test obj2 ray min-t)))))

;; CODE FOR intersect-hit-lists GOES HERE

(: intersect-hit-lists : Hit-List Hit-List -> Hit-List)
;; given two hit lists corresponding to two objects, compute the hit list for
;; the interesction of the two objects.
(define (intersect-hit-lists hits1 hits2)
  (match* (hits1 hits2)
    [('() _) '()]
    [(_ '()) '()]
        [((cons h1 r1) (cons h2 r2))
     (if (hit< h1 h2) 
         (local ;; 1st hit is closer
           {(define hits : Hit-List (intersect-hit-lists r1 hits2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) hits]
             [('IN 'OUT) (cons h1 hits)] ;; gets rid of h1
             [('OUT 'IN) hits]
             [('OUT 'OUT) (cons h1 hits)])) ;; gets rid of h1
         (local ;; second hit is closer
           {(define hits : Hit-List (intersect-hit-lists hits1 r2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) hits]
             [('IN 'OUT) hits]
             [('OUT 'IN) (cons h2 hits)]
             [('OUT 'OUT) (cons h2 hits)])))]))

;; CODE FOR object-intersect GOES HERE

(: object-intersect : Object Object -> Object)
;; return the intersection of two objects
(define (object-intersect obj1 obj2)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (intersect-hit-lists (hit-test obj1 ray min-t) (hit-test obj2 ray min-t)))))

;; CODE FOR subtract-hit-lists GOES HERE

(: subtract-hit-lists : Hit-List Hit-List -> Hit-List)
;; given two hit lists corresponding to two objects, compute the hit list for
;; the difference of the two objects.
(define (subtract-hit-lists hits1 hits2)
  (match* (hits1 hits2)
    [('() _) '()]
    [(_ '()) hits1]
        [((cons h1 r1) (cons h2 r2))
     (if (hit< h1 h2) 
         (local ;; 1st hit is closer
           {(define hits : Hit-List (subtract-hit-lists r1 hits2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) (cons h1 hits)]
             [('IN 'OUT) hits] ;; gets rid of h1
             [('OUT 'IN) (cons h1 hits)]
             [('OUT 'OUT) hits])) ;; gets rid of h1
         (local ;; second hit is closer
           {(define hits : Hit-List (subtract-hit-lists hits1 r2))}
           (match* ((Hit-parity h1) (Hit-parity h2))
             [('IN 'IN) hits]
             [('IN 'OUT) hits]
             [('OUT 'IN) (cons (flip-parity h2) hits)]
             [('OUT 'OUT) (cons (flip-parity h2) hits)])))]))

;; CODE FOR object-subtract GOES HERE

(: object-subtract : Object Object -> Object)
;; return the difference of two objects
(define (object-subtract obj1 obj2)
  (Object
   (lambda ([ray : Ray] [min-t : Float])
     (subtract-hit-lists (hit-test obj1 ray min-t) (hit-test obj2 ray min-t)))))

;;;;;;;;;;
;; Exports
;;;;;;;;;;

(provide object-union
         object-intersect
         object-subtract
         )