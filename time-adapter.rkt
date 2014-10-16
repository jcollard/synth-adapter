#lang racket

(require (prefix-in inst: "time.rkt")
         (prefix-in student: "time-two.rkt")
         "adapter.rkt")


(define (student->inst s-time)
  (let* ((seconds (student:time-seconds s-time))
         (hour (quotient seconds (* 60 60)))
         (minute (modulo (quotient seconds 60) 60))
         (second (modulo seconds 60)))
    (inst:time hour minute second)))

(define (inst->student i-time)
  (let* ((hour (inst:time-hour i-time))
         (minute (inst:time-minute i-time))
         (second (inst:time-second i-time))
         (seconds (+ (* hour 60 60) (* minute 60) second)))
    (student:time seconds)))

(define time-adapter (adapter student->inst
                              inst->student
                              student:time?
                              inst:time?))


        