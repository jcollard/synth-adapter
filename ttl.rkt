#lang s-exp rosette
(require "tcl+.rkt" 
         rosette/query/debug rosette/lib/tools/render
         rosette/lib/meta/meta rosette/lib/reflect/match)

(provide (all-defined-out) (all-from-out "tcl+.rkt") let
         match (rename-out [define define-transform]))

(define (verify-transform xform circ)
  (define input (symbolic-input circ))
  (define ast (apply circ input))
  (evaluate input (verify (assert (eq? (xform ast) ast)))))

(define (debug-transform xform circ bits)
  (define input (symbolic-input circ))
  (define ast (apply circ input))
  (render
   (debug [boolean?]
          (begin (assert (eq? (xform ast) ast))
                 (for ([in input] [bit bits])
                   (assert (eq? in bit)))))))

(define (synthesize-transform xform circ)
  (define input (symbolic-input circ))
  (define ast (apply circ input))
  (generate-forms
   (synthesize
    #:forall input
    #:guarantee (assert (eq? ast (xform ast))))))
