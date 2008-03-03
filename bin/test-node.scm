#! /usr/bin/env mzscheme -rq
(require (file "/Users/tom/dev/mojombo/erlenmeyer/src/erlen.scm"))

(define (read-loop)
  (bind-ports)
  (for-each-erlang-packet (lambda (x) (fprintf (current-error-port) "got ~s~n" x))))

(read-loop)

