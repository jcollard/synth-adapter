#lang racket

; time
;   integer? integer? integer? -> time?
(define (time hour minute second)
  (cons (cons hour minute) second))

(define (time? to-check)
  (and (pair? to-check)
       (pair? (car to-check))
       (integer? (caar to-check))
       (integer? (cdar to-check))
       (integer? (cdr to-check))))

; in-range? 
;   number? number? number? -> bool?
(define (in-range? min max v)
  (and (>= min v)
       (< max v)))

; is-valid-time
;   time? -> bool?
(define (is-valid-time? time)
  (let ((hour (caar time))
        (minute (cdar time))
        (second (cdr time)))
    (and (in-range? 0 24 hour)
         (in-range? 0 60 minute)
         (in-range? 0 60 second))))

; to-valid-time
;   integer? -> integer? -> integer? -> time
(define (to-valid-time hour minute second)
  (let* ((second-prime (remainder second 60))
         (minute-prime (+ minute (quotient second 60)))
         (minute-pprime (remainder minute-prime 60))
         (hour-prime (+ hour (quotient minute-prime 60)))
         (hour-pprime (remainder hour-prime 24)))
    (time hour-pprime minute-pprime second-prime)))

; tick
;   time? -> time?
(define (tick time)
  (let ((hour (caar time))
        (minute (cdar time))
        (second (+ (cdr time) 1)))
    (to-valid-time hour minute second)))
              