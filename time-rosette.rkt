#lang s-exp rosette

;(define-synthax (List #:depth d)
;  #:assert (>= d 0)
;  ([choose 

(require ;rosette/lang/debug 
         rosette/lib/tools/render
         rosette/lib/meta/meta)

(define-synthax (List #:depth d)
  #:assert (>= d 0)
  (cons 1 (List #:depth (- d 1))))


(define-synthax (Circuit [op1 op2 ...] expr ... #:depth d)
  #:assert (>= d 0)
  ([choose op1 identity]
   [choose
    expr ...
    ([choose op2 ...]
     (Circuit [op1 op2 ...] expr ... #:depth (- d 1))
     (Circuit [op1 op2 ...] expr ... #:depth (- d 1)))]))
  
(define (test)
  (Circuit ['x 'y] (cons 'x '()) #:depth 2))

(define (dynamic-bool)
  (define-symbolic* b boolean?)
  b)

(define (dynamic-number)
  (define-symbolic* n number?)
  n)


(define (synthesize-list generator #:depth [d 5])
  (assert (> d 0))
  (cond [(dynamic-bool) (cons (generator) (synthesize-list generator #:depth (- d 1)))]
        [else '()]))

(define (synth-list) 
  (synthesize-list (lambda () (dynamic-number))))


;(solve (assert (eq? '(1 2 3 4)
;                    (synth-list))))

;(define model
;  (synthesize
;   #:forall '()
;   #:guarantee (assert (equal? (synth-list)
;                               (synth-list)))))

