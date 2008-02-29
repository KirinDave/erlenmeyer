#! /usr/bin/env mzscheme -rq
(require (file "/Users/dfayram/Projects/erlenmeyer/src/erlenmeyer.scm"))

(define (read-loop)
  (bind-ports)
  (for-each-erlang-packet (lambda (x) (display "I got a golden ticket!\n"))))

(read-loop)

