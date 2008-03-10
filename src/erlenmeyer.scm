(module erlenmeyer mzscheme
  (require (lib "foreign.ss")
	   (lib "match.ss")) (unsafe!)
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
(define (read-stream n) (read-bytes n erlang-input-port))
(define (read-stream-i n) 
  (let ([data (read-bytes n erlang-input-port)])
    (cond
      ([eof-object? data] (raise 'unexpected-eof))
      (else data))))

; Frequently used functions in erlang binary takes a single unsigned byte and maps it to an int.
(define (byte->uint byte) (integer-bytes->integer (bytes-append #"\0" bytes) #f #t))
(define (bytes->int bytes) (integer-bytes->integer bytes #t #t))

; Specification language macro
(define-syntax define-binary-parser
  (syntax-rules ()
    [(_ name (byte-value processor-spec) ...)
     (define name 
       (lambda (bytes)
	 (let ([identifier (bytes-ref bytes 0)]
	       [dbytes (subbytes bytes 1)])
	   (let-syntax ((xform
	     (syntax-rules (small large custom ->)
	       [(_ (small -> byte-converter))   (parse-sized-entity dbytes 2 byte-converter)]
	       [(_ (large -> byte-converter))   (parse-sized-entity dbytes 4 byte-converter)]
	       [(_ (number -> byte-converter))  (parse-raw-entity dbytes number byte-converter)]
	       [(_ (custom raw-byte-converter)) (raw-byte-converter dbytes)])))
	   (fprintf (current-error-port) "In term parser! Bytes: ~s~n" bytes)
	   (cond 
	    [(equal? identifier byte-value) 
	       (fprintf (current-error-port) "Matched ~s to ~s~n" identifier byte-value)
	       (xform processor-spec)] ...
	     [else (raise `(unknown-data-type ,identifier ,bytes))])))))]))

; Parser helpers
(define (parse-raw-entity bytes size processor)
  (let [(rem-bytes  (subbytes bytes size))
	(data-bytes (subbytes bytes 0 size))]
    (cons (processor data-bytes) rem-bytes)))

(define (parse-sized-entity bytes size-field-length processor)
  (let* [(sizebytes    (subbytes bytes 0 size-field-length))
	 (size         (integer-bytes->integer sizebytes #f #t))]
    (parse-raw-entity (subbytes bytes size-field-length) size processor)))

; Data mappers

(define-binary-parser erlang-term-parser
  [ERL_SMALL_INT   (1 -> byte->uint)]
  [ERL_INT         (4 -> bytes->int)]
  [ERL_ATOM        (small -> (lambda (bytes) (string->symbol (bytes->string/utf-8 bytes))))]
  [ERL_STRING      (small -> (lambda (bytes) (bytes->string/utf-8 bytes)))]
  [ERL_BIN         (large -> (lambda (x) x))]
  [ERL_NIL         (custom (lambda (x) '()))]
  [ERL_LIST        (custom list-parser)]
  [ERL_SMALL_TUPLE (custom small-tuple-parser)]
)
;
; Recursive datatype parsers
;

; Tuples
(define (repeat-parser dbytes size-field-size size-field-converter finalizer)
  (let* [(sbytes  (subbytes dbytes 0 size-field-size))
	 (ebytes (subbytes dbytes size-field-size))
	 (nelems (size-field-converter sbytes))]
    ; input is: bytes, countdown, accumulated elements
    (letrec [(loop (lambda (b c accum)
		     (fprintf (current-error-port) "Inner loop (~s) with ~s~n" c accum)
		     (cond ((equal? c 0) (cons (finalizer (reverse accum)) b))
			   (else 
			     (match-let [((val . rbytes) (erlang-term-parser b))]
					(loop rbytes (sub1 c) (cons val accum)))))
	     ))]
      (loop ebytes nelems (list)))))

(define (small-tuple-parser dbytes) (repeat-parser dbytes 1 byte->uint (lambda (x) (list->vector x))))
(define (large-tuple-parser dbytes) (repeat-parser dbytes 4 bytes->int (lambda (x) (list->vector x))))
(define (list-parser dbytes) 
  (match-let [((val . rbytes) (repeat-parser dbytes 4 bytes->int (lambda (x) x)))]
	     (cons val (subbytes rbytes 1)))) ; Skip the trailing nil in lists.


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
      [(eof-object? size-record) (cons size-record #"")]
      [else (read-record size-record)])))

(define (for-each-erlang-packet lam)
  (display "Waiting on packet to read.\n")
  (match-let ([(packet . rembytes) (read-next-packet)])
    (cond 
      [(eof-object? packet) packet]
      [(> (bytes-length rembytes) 0) (raise `(unknown-extra-data ,rembytes ,packet))]
      [else (lam packet) (for-each-erlang-packet lam)])))
) ; End module