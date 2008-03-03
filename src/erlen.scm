(module erlen mzscheme
  (require (lib "foreign.ss")) (unsafe!)
  (provide for-each-erlang-packet
           bind-ports
           fd->input-port
           fd->output-port)

;  We'll start with the standard ports
  (define erlang-input-port (current-input-port))
  (define erlang-output-port (current-output-port))
 
  (define (bind-ports)
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
                  (_fun _int _scheme _int _int _int -> _scheme)))

;#include "ruby.h"
;#include <string.h>
;
;#define ERL_VERSION       131
;#define ERL_SMALL_INT     97
;#define ERL_INT           98
;#define ERL_SMALL_BIGNUM  110
;#define ERL_LARGE_BIGNUM  111
;#define ERL_FLOAT         99
;#define ERL_ATOM          100
;#define ERL_REF           101
;#define ERL_NEW_REF       114
;#define ERL_PORT          102
;#define ERL_PID           103
;#define ERL_SMALL_TUPLE   104
;#define ERL_LARGE_TUPLE   105
;#define ERL_NIL           106
;#define ERL_STRING        107
;#define ERL_LIST          108
;#define ERL_BIN           109
;#define ERL_FUN           117
;#define ERL_NEW_FUN       112

(define ERL_VERSION       #"\203")
(define ERL_SMALL_INT     #"\141")
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

;static VALUE mErlectricity;
;static VALUE cDecoder;
;void Init_decoder();
;
;VALUE method_read_any_from(VALUE klass, VALUE rString);
;
;VALUE read_any_raw(unsigned char **pData);
;
;// checkers
;
;void check_int(int num) {
;  char buf[17];
;  sprintf(buf, "%u", num);
;  rb_raise(rb_eStandardError, buf);
;}
;
;void check_str(char *str) {
;  rb_raise(rb_eStandardError, str);
;}
;
;// string peekers/readers
;
;unsigned int peek_1(unsigned char **pData) {
;  return (unsigned int) **pData;
;}

(define (peek-1 data)
  (peek-bytes 1 0 data)
)

;unsigned int peek_2(unsigned char **pData) {
;  return (unsigned int) ((**pData << 8) + *(*pData + 1));
;}

(define (peek-2 data)
  (peek-bytes 2 0 data)
)

;unsigned int peek_4(unsigned char **pData) {
;  return (unsigned int) ((**pData << 24) + (*(*pData + 1) << 16) + (*(*pData + 2) << 8) + *(*pData + 3));
;}
;
;unsigned int read_1(unsigned char **pData) {
;  unsigned int val = peek_1(pData);
;  *pData += 1;
;  return val;
;}

(define (read-1 data)
  (read-bytes 1 data)
)

;unsigned int read_2(unsigned char **pData) {
;  unsigned int val = peek_2(pData);
;  *pData += 2;
;  return val;
;}
;
;unsigned int read_4(unsigned char **pData) {
;  unsigned int val = peek_4(pData);
;  *pData += 4;
;  return val;
;}

(define (read-4 data)
  (read-bytes 4 data)
)

;// tuples, lists
;
;VALUE read_small_tuple(unsigned char **pData) {
;  if(read_1(pData) != ERL_SMALL_TUPLE) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a small tuple");
;  }
;  
;  int arity = read_1(pData);
;  
;  VALUE array = rb_ary_new2(arity);
;  
;  int i;
;  for(i = 0; i < arity; ++i) {
;    rb_ary_store(array, i, read_any_raw(pData));
;  }
;  
;  return array;
;}
;
;VALUE read_large_tuple(unsigned char **pData) {
;  if(read_1(pData) != ERL_LARGE_TUPLE) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a large tuple");
;  }
;  
;  int arity = read_4(pData);
;  
;  VALUE array = rb_ary_new2(arity);
;  
;  int i;
;  for(i = 0; i < arity; ++i) {
;    rb_ary_store(array, i, read_any_raw(pData));
;  }
;  
;  return array;
;}
;
;VALUE read_list(unsigned char **pData) {
;  if(read_1(pData) != ERL_LIST) {
;    rb_raise(rb_eStandardError, "Invalid Type, not an erlang list");
;  }
;  
;  int size = read_4(pData);
;  
;  VALUE array = rb_ary_new2(size);
;  
;  int i;
;  for(i = 0; i < size; ++i) {
;    rb_ary_store(array, i, read_any_raw(pData));
;  }
;  
;  return array;
;}
;
;// primitives
;
;void read_string_raw(unsigned char *dest, unsigned char **pData, int length) {
;  memcpy((char *) dest, (char *) *pData, length);
;  *(dest + length) = (unsigned char) 0;
;  *pData += length;
;}
;
;VALUE read_bin(unsigned char **pData) {
;  if(read_1(pData) != ERL_BIN) {
;    rb_raise(rb_eStandardError, "Invalid Type, not an erlang binary");
;  }
;  
;  int length = read_4(pData);
;  
;  unsigned char buf[length + 1];
;  read_string_raw(buf, pData, length);
;  
;  return rb_str_new2((char *) buf);
;}
;
;VALUE read_string(unsigned char **pData) {
;  if(read_1(pData) != ERL_STRING) {
;    rb_raise(rb_eStandardError, "Invalid Type, not an erlang string");
;  }
;  
;  int length = read_2(pData);
;  
;  unsigned char buf[length + 1];
;  read_string_raw(buf, pData, length);
;  
;  VALUE array = rb_ary_new2(length);
;  
;  int i = 0;
;  for(i; i < length; ++i) {
;    rb_ary_store(array, i, INT2NUM(*(buf + i)));
;  }
;  
;  return array;
;}
;
;VALUE read_atom(unsigned char **pData) {
;  if(read_1(pData) != ERL_ATOM) {
;    rb_raise(rb_eStandardError, "Invalid Type, not an atom");
;  }
;  
;  int length = read_2(pData);
;  
;  unsigned char buf[length + 1];
;  read_string_raw(buf, pData, length);
;  
;  return ID2SYM(rb_intern((char *) buf));
;}
;
;VALUE read_small_int(unsigned char **pData) {
;  if(read_1(pData) != ERL_SMALL_INT) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a small int");
;  }
;  
;  int value = read_1(pData);
;  
;  return INT2FIX(value);
;}

(define (read-small-int data)
  (fprintf (current-error-port) "got small-int ~s~n" (peek-2 data))
  
  (unless (equal? (read-1 data) ERL_SMALL_INT)
    (raise 'invalid-type-not-small-int)
  )
  (integer-bytes->integer (bytes-append #"\0" (read-1 data)) #f #t)
)

;VALUE read_int(unsigned char **pData) {
;  if(read_1(pData) != ERL_INT) {
;    rb_raise(rb_eStandardError, "Invalid Type, not an int");
;  }
;  
;  long long value = read_4(pData);
;  
;  long long negative = ((value >> 31) & 0x1 == 1);
;  
;  if(negative) {
;    value = (value - ((long long) 1 << 32));
;  }
;  
;  return INT2FIX(value);
;}
;
;VALUE read_small_bignum(unsigned char **pData) {
;  if(read_1(pData) != ERL_SMALL_BIGNUM) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a small bignum");
;  }
;  
;  unsigned int size = read_1(pData);
;  unsigned int sign = read_1(pData);
;  
;  VALUE num = INT2NUM(0);
;  VALUE tmp;
;  
;  unsigned char buf[size + 1];
;  read_string_raw(buf, pData, size);
;  
;  int i;
;  for(i = 0; i < size; ++i) {
;    tmp = INT2FIX(*(buf + i));
;    tmp = rb_funcall(tmp, rb_intern("<<"), 1, INT2NUM(i * 8));
;    num = rb_funcall(num, rb_intern("+"), 1, tmp);
;  }
;  
;  if(sign) {
;    num = rb_funcall(num, rb_intern("*"), 1, INT2NUM(-1));
;  }
;  
;  return num;
;}
;
;VALUE read_large_bignum(unsigned char **pData) {
;  if(read_1(pData) != ERL_LARGE_BIGNUM) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a small bignum");
;  }
;  
;  unsigned int size = read_4(pData);
;  unsigned int sign = read_1(pData);
;  
;  VALUE num = INT2NUM(0);
;  VALUE tmp;
;  
;  unsigned char buf[size + 1];
;  read_string_raw(buf, pData, size);
;  
;  int i;
;  for(i = 0; i < size; ++i) {
;    tmp = INT2FIX(*(buf + i));
;    tmp = rb_funcall(tmp, rb_intern("<<"), 1, INT2NUM(i * 8));
;    
;    num = rb_funcall(num, rb_intern("+"), 1, tmp);
;  }
;  
;  if(sign) {
;    num = rb_funcall(num, rb_intern("*"), 1, INT2NUM(-1));
;  }
;  
;  return num;
;}
;
;VALUE read_float(unsigned char **pData) {
;  if(read_1(pData) != ERL_FLOAT) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a float");
;  }
;  
;  unsigned char buf[32];
;  read_string_raw(buf, pData, 31);
;  
;  VALUE rString = rb_str_new2((char *) buf);
;  
;  return rb_funcall(rString, rb_intern("to_f"), 0);
;}
;
;VALUE read_nil(unsigned char **pData) {
;  if(read_1(pData) != ERL_NIL) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a nil list");
;  }
;  
;  return rb_ary_new2(0);
;}
;
;// specials
;
;VALUE read_pid(unsigned char **pData) {
;  if(read_1(pData) != ERL_PID) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a pid");
;  }
;  
;  VALUE node = read_atom(pData);
;  VALUE id = INT2NUM(read_4(pData));
;  VALUE serial = INT2NUM(read_4(pData));
;  VALUE creation = INT2FIX(read_1(pData));
;  
;  VALUE pid_class = rb_const_get(mErlectricity, rb_intern("Pid"));
;  return rb_funcall(pid_class, rb_intern("new"), 4, node, id, serial, creation);
;}
;
;VALUE read_new_reference(unsigned char **pData) {
;  if(read_1(pData) != ERL_NEW_REF) {
;    rb_raise(rb_eStandardError, "Invalid Type, not a new-style reference");
;  }
;  
;  int size = read_2(pData);
;  VALUE node = read_atom(pData);
;  VALUE creation = INT2FIX(read_1(pData));
;  
;  VALUE id = rb_ary_new2(size);
;  int i;
;  for(i = 0; i < size; ++i) {
;    rb_ary_store(id, i, INT2NUM(read_4(pData)));
;  }
;  
;  VALUE newref_class = rb_const_get(mErlectricity, rb_intern("NewReference"));
;  return rb_funcall(newref_class, rb_intern("new"), 3, node, creation, id);
;}
;
;// read_any_raw
;
;VALUE read_any_raw(unsigned char **pData) {
;  switch(peek_1(pData)) {
;    case ERL_SMALL_INT:
;      return read_small_int(pData);
;      break;
;    case ERL_INT:
;      return read_int(pData);
;      break;
;    case ERL_FLOAT:
;      return read_float(pData);
;      break;
;    case ERL_ATOM:
;      return read_atom(pData);
;      break;
;    case ERL_PID:
;      return read_pid(pData);
;      break;
;    case ERL_SMALL_TUPLE:
;      return read_small_tuple(pData);
;      break;
;    case ERL_LARGE_TUPLE:
;      return read_large_tuple(pData);
;      break;
;    case ERL_NIL:
;      return read_nil(pData);
;      break;
;    case ERL_STRING:
;      return read_string(pData);
;      break;
;    case ERL_LIST:
;      return read_list(pData);
;      break;
;    case ERL_BIN:
;      return read_bin(pData);
;      break;
;    case ERL_SMALL_BIGNUM:
;      return read_small_bignum(pData);
;      break;
;    case ERL_LARGE_BIGNUM:
;      return read_large_bignum(pData);
;      break;
;    case ERL_NEW_REF:
;      return read_new_reference(pData);
;      break;
;  }
;  return Qnil;
;}

(define (read-any-raw data)
  ; (fprintf (current-error-port) "data type ~s = ~s~n" ERL_SMALL_INT (peek-1 data))
  (let ((data-type (peek-1 data)))
    (
      [cond 
        ((equal? data-type ERL_SMALL_INT) [read-small-int data])
        (else ())
      ]
    )
  )
)

;VALUE method_read_any_from(VALUE klass, VALUE rString) {
;  unsigned char *data = (unsigned char *) StringValuePtr(rString);
;  
;  unsigned char **pData = &data;
;  
;  // check protocol version
;  if(read_1(pData) != ERL_VERSION) {
;    rb_raise(rb_eStandardError, "Bad Magic");
;  }
;  
;  return read_any_raw(pData);
;}

(define (read-any-from data)
  (read-4 data)
  [unless (equal? (read-1 data) ERL_VERSION)
    (raise 'bad-version)
  ]
  (read-any-raw data)
)

; in erlectricity this is outside the C extension in the Ruby code

(define (for-each-erlang-packet lam)
  (display "Waiting on packet to read.\n")
  (let ((packet (read-any-from erlang-input-port)))
    (
      ; (fprintf (current-error-port) "packet is ~s~n" packet)
      (lam packet)
      (for-each-erlang-packet lam)
    )
  )
)

) ; module