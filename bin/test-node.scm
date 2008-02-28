#! /usr/bin/env mzscheme -rq
(require (file "/Users/dfayram/Projects/erlenmeyer/lib/erlenmeyer.zo"))

(define (read-loop)
  (read-next-packet)
  (read-loop))

(bind-erlang-ports)
(read-loop)

