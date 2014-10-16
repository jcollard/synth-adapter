#lang racket

;; An adapter provides functions for converting between
;; student and instructor structs
(provide (struct-out adapter))
(struct adapter (student->inst     
                 inst->student
                 student-struct?
                 instructor-struct?))

; adapter? -> ((U adapter-student-struct? adapter-instructor-struct?) -> boolean?)
; Checks that conversions to and from student / instructor structs is equivalent
(provide check-equality?)
(define (check-equality? adapter)
  (let ((student-struct? (adapter-student-struct? adapter))
        (instructor-struct? (adapter-instructor-struct? adapter))
        (student->inst (adapter-student->inst adapter))
        (inst->student (adapter-inst->student adapter)))
    (lambda (struct)
      (cond [(instructor-struct? struct) (equal? struct
                                                 (student->inst (inst->student struct)))]
            [(student-struct? struct) (equal? struct
                                              (inst->student (student->inst struct)))]))))

;(: check-function-equiv? (adapter? 
;                          (instructor-struct? -> Any) 
;                          (student-struct? -> Any) -> ((U instructor-struct? student-struct?) -> boolean?)))
; Checks the result of a student function against the result of the instructor function
(provide check-function-equiv?)
(define (check-function-equiv? adapter inst-f student-f)
  (let* ((student->inst (adapter-student->inst adapter))
         (inst->student (adapter-inst->student adapter))
         (student:check (compose inst->student inst-f student->inst))
         (inst:check (compose student->inst student-f inst->student))
         (student-struct? (adapter-student-struct? adapter))
         (instructor-struct? (adapter-instructor-struct? adapter)))
    (lambda (time)
      (cond [(instructor-struct? time) (equal? (inst-f time)
                                               (inst:check time))]
            [(student-struct? time) (equal? (student-f time)
                                            (student:check time))]))))
