#lang racket


(provide (struct-out time))
(struct time (seconds) #:transparent)

; in-range? 
;   number? number? number? -> bool?
(define (in-range? min max v)
  (and (>= min v)
       (< max v)))

; is-valid-time
;   time? -> bool?
(provide is-valid-time?)
(define (is-valid-time? time)
    (in-range? 0 86400 (time-seconds time)))

; tick
;   time? -> time?
(provide tick)
(define (tick t)
  (let ((seconds (time-seconds t)))
    (time (modulo (+ seconds 1) 86400))))
              