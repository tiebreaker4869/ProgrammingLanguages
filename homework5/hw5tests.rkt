#lang racket

(require "hw5.rkt")

; This file uses Racket's unit-testing framework, which is convenient but not required of you.

; Note we have provided [only] 3 tests, but you can't run them until do some of the assignment.
; You will want more tests.

(require rackunit)

(define tests
  (test-suite
   "Homework 5 Tests"

   (check-equal? (eval-exp (add (int 2) (int 2))) (int 4) "add simple test")

   (check-exn (lambda (x) (string=? (exn-message x) "MUPL addition applied to non-number"))
              (lambda () (eval-exp (add (int 2) (munit))))
              "add bad argument")

   (check-equal? (mupllist->racketlist
                  (eval-exp (call (call mupl-all-gt (int 9))
                                  (racketlist->mupllist 
                                   (list (int 10) (int 9) (int 15))))))
                 (list (int 10) (int 15))
                 "provided combined test using problems 1, 2, and 4")
   (let ([program (call (mlet "x" (int 1) (fun null "_i" (add (var "x") (var "_i")))) (int 5))])
     (check-equal? (eval-exp program) (eval-exp-c program)))

   (let ([program
          (call (fun "_sum_n" "_n" (ifnz (var "_n") (add (var "_n") (call (var "_sum_n") (add (var "_n") (int -1)))) (int 0))) (int 5))])
     (check-equal? (eval-exp program) (eval-exp-c program)))
   ))

(require rackunit/text-ui)
;; runs the test
(run-tests tests)
