(module erlenmeyer mzscheme
  (require (lib "foreign.ss")) (unsafe!)
  (provide bind-erlang-ports
           fd->input-port
           fd->output-port)

;  We'll start with the standard ports
  (define erlang-input-port (current-input-port))
  (define erlang-output-port (current-output-port))
 
 (define (bind-erlang-ports)
   (set! erlang-input-port  (fd->input-port 3 'erlang-in))
   (set! erlang-output-port (fd->output-port 4 'erlang-out)))
  
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
                  (_fun _int _scheme _int _int _int -> _scheme))))