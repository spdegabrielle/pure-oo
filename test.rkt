#lang racket/gui
(require 2htdp/image "pureoo.rkt")

; point-2D object and its constructor

(define (make-point-2D x y)
  (define (my-identity) message-map)
  (define message-map
    `((get-x . ,(lambda (self) (list self x)))
      (get-y . ,(lambda (self) (list self y)))
      (set-x . ,(lambda (self new-x)
		  (list
		   (make-dispatcher
		    (new-mmap (self 'mmap) 'get-x
			      (lambda (self) (list self new-x)))))))
      (set-y . ,(lambda (self new-y)
		  (list
		   (make-dispatcher
		    (new-mmap (self 'mmap) 'get-y
			      (lambda (self) (list self new-y)))))))
      (identity . ,(lambda (self) (list self my-identity)))
      (my-class . ,(lambda (self) (list self "point-2D")))
      (of-class . ,(lambda (self) (self 'my-class))) ; an example of sending a message to myself
      ))
  (make-dispatcher message-map))

(define (print-x-y obj)
  ;; rev-apply make this easier to read
  (define (rev-apply lst handler) (apply handler lst))
  (rev-apply (obj 'get-x)
             (lambda (obj x)
               (rev-apply (obj 'get-y)
                          (lambda (obj y)
                            (rev-apply (obj 'parent*)
                                       (lambda (obj parent*)
                                         (list parent* x y))))))))

(displayln "test output 1")
(displayln "define p and p1")
(define p (make-point-2D 5 6))
(define p1 (make-point-2D 5 6))

(displayln "test output 2")
(p1 'of-class) ; ==> (#<procedure p1> "point-2D")
(p1 'parent*)
(p 'get-x)     ; ==> (#<procedure p> 5)
(p1 'get-x)    ; ==> (#<procedure p1> 5)

(displayln "(print-x-y p)")
(print-x-y p)  ; ==> ("point-2D" 5 6)
; p and p1 happen at this point to be in the same state
(equal? (print-x-y p) (print-x-y p1)) ; ==> #t

; but p and p1 are different, non-identical objects
(eq? (cadr (p 'identity)) (cadr (p1 'identity))) ; ==> #f

; pm is the object 'p' after the "mutation"
; Note that closures 'pm' and 'p' _share_ all the common state,
; including their identity
(define pm (car (p 'set-x 10)))
(print-x-y pm) ; ==> ("point-2D" 10 6)

; States differ, identities are the same
(equal? (print-x-y p) (print-x-y pm)) ; ==> #f
(eq? (cadr (p 'identity)) (cadr (pm 'identity))) ; ==> #t

; Illustrating inheritance and polymorphism
; A derived "object" inherits the message-map of its parent*, and
; adds its own handlers at the top. Because subclass' handlers will
; be encountered first when a handler for a selector is being
; searched, the subclass is able to override the behavior of its
; superclass.

(define (make-point-3D x y z)
  (define message-map
    (append
     `((get-z . ,(lambda (self) (list self z)))
       (set-z . ,(lambda (self new-z)
                   (list
                    (make-dispatcher
                     (new-mmap (self 'mmap) 'get-z
                               (lambda (self) (list self new-z)))))))
       (me . ,(lambda (self) (list self "point-3D")))
       )
     ((make-point-2D x y) 'mmap)))	; the superclass

  (make-dispatcher message-map))


(define q (make-point-3D 1 2 3))

; Although print-x-y was defined for objects of type point-2D,
; it accepts point-3D objects as well. Note however that the head
; of print-x-y's result spells "point-3D". This is because a point-3D
; object overrides the message 'parent* of its parent* class
(print-x-y q)  ; ==> ("point-3D" 1 2)
(q 'get-z)     ; ==> (#<procedure q> 3)

; Demonstrating the use of inherited methods....
(let ((obj (car ((car (q 'set-x 11)) 'set-z 12))))
  (append (print-x-y obj) (cdr (obj 'get-z))))
; ==> ("point-3D" 11 2 12) 

(q 'parent*)
; ==> (#<procedure q> "point-3D") ; notice polymorphism!!!
; The of-class method is implemented in point-2D yet the result is
; "point-3D". This is because the 'of-class method sends the message
; 'parent* to itself. The latter handler is over-ridden in a
; class point-3D to return its own title, "point-3D".

(displayln "(q 'parent*)")
(q 'parent*)
(displayln "(q 'mmap)")
(q 'mmap)
(displayln "(q 'slots)")
(q 'slots)

;(q 'slotxs)
