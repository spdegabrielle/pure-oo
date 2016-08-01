#lang racket
(provide make-dispatcher new-mmap core-obj)
; a simple direct port of http://okmij.org/ftp/Scheme/#pure-oo ...
;
; 	Purely-functional Object-Oriented System in Scheme
;
; The present code implements a classless, delegation-based OO system, similar
; to those of Self or Javascript. This is a full-fledged OO system with
; encapsulation, object identity, inheritance and polymorphism. It is also
; a purely functional system: there is not a single assignment or
; other mutation in the code below.
;
; A closure (encapsulating a message map, and private parameters if any)
; is the object in this system. Sending a message to an object -- i.e.,
; applying the object to a message selector and arguments, -- results
; in a list. Its head is the object in a new state, having processed the
; message; the rest of the list are the results of the message if any.
; Objects' identity is decided by an eq? predicate applied to the result of
; an 'identity' message. A "set-x" method returns an object with a new state,
; but with the same identity as the source object. An object in a changed
; state is in a sense a "child" of the original object. No wonder
; implementations of "mutation" and inheritance are so similar in this
; OO system.
;
; This code was meant to be "light"; therefore it deliberately uses only the
; most basic Scheme constructions. No macros/syntactic closures are employed
; (although they are certainly possible and appropriate).
;
; This code has been discussed in an article "Re: FP, OO and relations. Does
; anyone trump the others?"
; posted on comp.lang.smalltalk, comp.lang.functional, and comp.lang.scheme
; on Wed Dec 29 05:13:58 1999 GMT, Message-ID: <84c4qe$48j$1@nnrp1.deja.com>
; See also http://pobox.com/~oleg/ftp/Scheme/oop-in-fp.txt
; The article presents a more familiar OO system with mutations, and contrasts
; it with a purely-functional OO system, which is implemented in this present
; file.
;
; $Id: pure-oo-system.scm,v 1.2 2000/03/01 02:50:40 oleg Exp oleg $


; A functional substitution in a assoc list
(define (new-mmap mmap tag new-body)
  (cond
    ((null? mmap) '())
    ((eq? tag (caar mmap))	; replacement
     (cons (cons tag new-body) (cdr mmap)))
    (else (cons (car mmap) (new-mmap (cdr mmap) tag new-body)))))



; This function makes a new dispatcher closure -- a new
; object: a dispatcher _is_ an object.
; A message map is a list of associations of message
; selectors with message handlers. A message selector
; is a symbol. A message handler is a procedure. It should
; take the dispatcher (i.e., _self_) as the first argument,
; and message's arguments as other parameters, if any.
; Every object always accepts a message 'mmap and replies with
; its message map. This feature is used to create an
; object with a new state, and to implement a delegation-based
; inheritance (see make-point-3D below). The similarity
; between the two runs deeper: an object with a changed state
; is in a sense a "child" of the original object.
;; make-dispatcher : message-map > 
(define (make-dispatcher message-map)
  (define (dispatcher selector . args)
    (cond
      ((eq? selector 'slots) (map (λ (message-pair) (car message-pair))
                                  message-map))
      ((eq? selector 'mmap) message-map)
      ((assq selector message-map) =>
                                   (lambda (handler-ass)
                                     (apply (cdr handler-ass) (cons dispatcher args))))
      ((eq? selector 'parent*) (list dispatcher "UNKNOWN")) ; if wasn't defined
      (else						     ; in message-map
       (raise-argument-error (string->symbol (cadr (dispatcher 'parent*)))
                             (format ":~a:" (map (λ (message-pair) (car message-pair))
                                                 message-map))
                             0
                             selector
                             ))
      ))
  dispatcher)


;make core-object object
;; core-obj > core-obj
(define (core-obj)
  (define (my-identity) message-map)
  (define message-map
    `((parent* . ,(lambda (self) (list self "core-object")))
      (of-class . ,(lambda (self) (self 'parent*))) ; an example of
      ; sending a message to myself
      ))
  (make-dispatcher message-map))

