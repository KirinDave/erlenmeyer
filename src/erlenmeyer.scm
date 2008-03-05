(module erlenmeyer mzscheme
  (require (lib "foreign.ss")) (unsafe!)
  (provide for-each-erlang-packet
           read-next-packet 
           fd->input-port
           fd->output-port)



; The all-important fd->input port code thanks to Matthew Flatt
(define (fd->input-port fd name)
  (scheme_make_fd_input_port fd name 0 0))

(define (fd->output-port fd name)
  (scheme_make_fd_output_port fd name 0 0 0))

(define scheme_make_fd_input_port
  (get-ffi-obj "scheme_make_fd_input_port" #f
               (_fun _int _scheme _int _int -> _scheme)))

(define scheme_make_fd_output_port
  (get-ffi-obj "scheme_make_fd_output_port"
                #f
                (_fun _int _scheme _int _int _int -> _scheme)))

;  We'll start with the standard ports
(define erlang-input-port (fd->input-port 3 'erlang-in))
(define erlang-output-port (fd->output-port 4 'erlang-out))

; Data conversion functions
(define ERL_MAGIC_NUMBER  #"\203")
(define ERL_SMALL_INT     97)
(define ERL_INT           98)
(define ERL_SMALL_BIGNUM  110)
(define ERL_LARGE_BIGNUM  111)
(define ERL_FLOAT         99)
(define ERL_ATOM          100)
(define ERL_REF           101)
(define ERL_NEW_REF       114)
(define ERL_PORT          102)
(define ERL_PID           103)
(define ERL_SMALL_TUPLE   104)
(define ERL_LARGE_TUPLE   105)
(define ERL_NIL           106)
(define ERL_STRING        107)
(define ERL_LIST          108)
(define ERL_BIN           109)
(define ERL_FUN           117)
(define ERL_NEW_FUN       112)

; Reading primitives
(define (peek-1 data) (peek-bytes 1 0 data))
(define (peek-2 data) (peek-bytes 2 0 data))
(define (read-1 data) (read-bytes 1 data))
(define (read-4 data) (read-bytes 4 data))

(define (read-stream n) (read-bytes n erlang-input-port))
(define (read-stream-i n) 
  (let ([data (read-bytes n erlang-input-port)])
    (cond
      ([eof-object? data] (raise 'unexpected-eof))
      (else data))))

(define-syntax define-binary-parser
  (syntax-rules ()
    [(_ name (byte-value processor) ...)
     (define name 
       (lambda (bytes)
         (let ([identifier (bytes-ref bytes 0)])
          (fprintf (current-error-port) "In term parser! Bytes: ~s~n" bytes)
          (cond 
            [(equal? identifier byte-value) 
              (fprintf (current-error-port) "Matched ~s to ~s~n" identifier byte-value)
              (processor (subbytes bytes 1))] ...
            [else (raise `(unknown-data-type ,identifier ,bytes))]))))
	  ]
	)
)

; Data mappers

(define-binary-parser erlang-term-parser
  [ERL_SMALL_INT (lambda (bytes) (integer-bytes->integer (bytes-append #"\0" bytes) #f #t))]
  [ERL_INT (lambda (bytes) (integer-bytes->integer bytes #t #t))]
  [ERL_ATOM (lambda (bytes) (string->symbol (bytes->string/utf-8 (subbytes bytes 2))))]
  [ERL_STRING (lambda (bytes) (bytes->string/utf-8 (subbytes bytes 2)))]
)

(define (read-size-field)
  (let ([size-bytes (read-stream 4)])
    (cond 
      [(eof-object? size-bytes) size-bytes]
      [else (integer-bytes->integer size-bytes #f #t)])))

(define (read-magic-number)
  (let ([mnum (read-stream-i 1)])
    (unless (equal? ERL_MAGIC_NUMBER mnum) 
      (raise `(bad-magic ,mnum)))
    mnum))

(define (read-record size)
  (let* ([mnum       (read-magic-number)]
         [data-bytes (read-stream-i (sub1 size))])
     (fprintf (current-error-port) "RECORD (~s): ~s~n" mnum data-bytes)
     (erlang-term-parser data-bytes)))

(define (read-next-packet)
  (let ([size-record (read-size-field)])
    (cond 
      [(eof-object? size-record) eof]
      [else (read-record size-record)])))

(define (for-each-erlang-packet lam)
  (display "Waiting on packet to read.\n")
  (let ([packet (read-next-packet)])
    (cond 
      [(eof-object? packet) packet]
      [else (lam packet) (for-each-erlang-packet lam)])))



) ; End module