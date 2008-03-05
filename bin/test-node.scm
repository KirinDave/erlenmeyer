#! /usr/bin/env mzscheme -rq
(require (file "../src/erlenmeyer.scm"))

(define (read-loop)
  (for-each-erlang-packet (lambda (x) (fprintf (current-error-port) "got ~s~n" x))))

(read-loop)

