#! /usr/bin/env mzscheme -rq
(require (file "../src/erlen.scm"))

(define (read-loop)
  (bind-ports)
  (for-each-erlang-packet (lambda (x) (fprintf (current-error-port) "got ~s~n" x))))

(read-loop)

