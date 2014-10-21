#lang s-exp rosette

(require rosette/query/debug rosette/lib/tools/render
         rosette/lib/meta/meta)

(provide (all-defined-out) ! && || <=> define/debug
         #%datum #%app #%module-begin #%top-interaction
         quote (for-syntax #%datum))

(define-syntax-rule (define-circuit (id in ...) expr)
  (define (id in ...) expr))

(define (symbolic-input spec)
  (for/list ([i (procedure-arity spec)]) (dynamic-choose)))

(define (correct impl spec input)
   (assert (eq? (apply impl input) (apply spec input))))

(define (verify-circuit impl spec)
  (define input (symbolic-input spec))
  (evaluate input (verify (correct impl spec input))))

(define (debug-circuit impl spec input)
  (render (debug [boolean?] (correct impl spec input))))

(define (solve-circuit impl spec . inputs)
  (solve (for ([input inputs]) (correct impl spec input))))

(define (synthesize-circuit impl spec)
  (define input (symbolic-input spec))
  (generate-forms
   (synthesize #:forall input
               #:guarantee (correct impl spec input))))

(define-synthax (Circuit [op1 op2 ...] expr ... #:depth d)
  #:assert (>= d 0)
  ([choose op1 identity]
   [choose
    expr ...
    ([choose op2 ...]
 (Circuit [op1 op2 ...] expr ... #:depth (- d 1))
 (Circuit [op1 op2 ...] expr ... #:depth (- d 1)))]))

(define-symbolic b0 b1 b2 b3 boolean?)
(define (dynamic-choose)
  (define-symbolic* v boolean?)
  v)

(define-circuit (RBC-parity a b c d)
  (xor (<=> a b) (<=> c d)))

(define-circuit (AIG-parity a b c d)
  (&&
   (Circuit [! &&] a b c d #:depth 3)
   (! (&& (&& (! (&& a b)) (! (&& (! a) (! b))))
          (&& (! (&& (! c) (! d))) (! (&& c d)))))))

(define model 
  (synthesize
   #:forall (list b0 b1 b2 b3)
   #:guarantee (assert (eq? (AIG-parity b0 b1 b2 b3)
                            (RBC-parity b0 b1 b2 b3)))))

; (generate-forms model)



