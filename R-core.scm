(foreign-declare
 #<<END
 #include <Rembedded.h>
 #include <Rinternals.h>
END
)

(let ((r-home
       (or (get-environment-variable "R_HOME")
           (begin
             (warning "R_HOME not set: running `R RHOME'; better set R_HOME.")
             (string-trim-right (capture (R RHOME)))))))
  (call-with-environment-variables
   `(("R_HOME" . ,r-home))
   (lambda ()
     (foreign-code
      #<<END
      Rf_initEmbeddedR(4, (char*[]) {"R-less-c",
                                     "--slave",
                                     "--vanilla",
                                     "--args"}) ;
END
))))

(define (named×unnamed arguments)
  (let iter ((arguments arguments)
             (keyword #f)
             (named '())
             (unnamed '()))
    (if (null? arguments)
        (values (reverse named) (reverse unnamed))
        (let ((argument (car arguments))
              (rest (cdr arguments)))
          (if (keyword? argument)
              (iter rest argument named unnamed)
              (if keyword
                  (iter rest
                        #f
                        (alist-cons (keyword->string keyword)
                                    argument named)
                        unnamed)
                  (iter rest #f named (cons argument unnamed))))))))

(define-foreign-type SEXP
  (c-pointer "SEXP")
  values
  ;; Can we cast here, too, so we don't have to keep doing `(SEXP)s'
  ;; all over the place?
  ;;
  ;; What if we tagged pointers like R-int, &c.?
  (lambda (sexp)
    ((foreign-lambda*
      void
      ((SEXP sexp))
      "R_PreserveObject((SEXP) sexp);")
     sexp)
    (set-finalizer!
     (tag-pointer sexp 'sexp)
     (lambda (sexp)
       ((foreign-lambda*
         void
         ((SEXP sexp))
         "R_ReleaseObject((SEXP) sexp);")
        sexp)))))

(define R-null (foreign-value "R_NilValue" SEXP))

(define R-missing
  @("R-constant for missing arguments")
  (foreign-value "R_MissingArg" SEXP))

(define R-null
  @("NULL")
  (foreign-value "R_NilValue" SEXP))

(define (R-symbol symbol)
  ((foreign-lambda*
    SEXP
    ((c-string symbol))
    "C_return(Rf_install(symbol));")
   (symbol->string symbol)))

(define (R-boolean boolean)
  ((foreign-lambda*
    SEXP
    ((bool bool))
    "C_return(ScalarLogical(bool ? 1 : 0));")
   boolean))

(define (R-integer integer)
  ((foreign-lambda*
    SEXP
    ((int integer))
    "C_return(ScalarInteger(integer));")
   integer))

(define (R-real real)
  ((foreign-lambda*
    SEXP
    ((double real))
    "C_return(ScalarReal(real));")
   real))

(define (R-complex real imaginary)
  ((foreign-lambda*
    SEXP
    ((double real)
     (double imaginary))
    "SEXP complex = allocVector(CPLXSXP, 1);"
    "COMPLEX(complex)[0].r = real;"
    "COMPLEX(complex)[0].i = imaginary;"
    "C_return(complex);")
   real
   imaginary))

(define (R-string string)
  ((foreign-lambda*
    SEXP
    ((c-string string))
    "C_return(ScalarString(Rf_mkChar(string)));")
   string))

(define (R-vector vector)
  (R-apply 'c (vector->list vector)))

(define (R-list list)
  (R-apply 'list list))

(define (sexp-pointer? object)
  (tagged-pointer? object 'sexp))

(define (scheme->R value)
  (type-case* value
    (null R-null)
    (symbol (R-symbol it))
    (boolean (R-boolean it))
    (integer (R-integer it))
    ((real rational) (R-real it))
    (complex (R-complex (real-part it) (imag-part it)))
    (string (R-string it))
    (vector (R-vector it))
    (sexp-pointer it)
    (list (R-list it))
    (else (error "Can't translate value -- scheme->R" value))))

;; (trace scheme->R)

(define-foreign-variable R-null-type int "NILSXP")
(define-foreign-variable R-symbol-type int "SYMSXP")
(define-foreign-variable R-boolean-type int "LGLSXP")
(define-foreign-variable R-integer-type int "INTSXP")
(define-foreign-variable R-real-type int "REALSXP")
(define-foreign-variable R-complex-type int "CPLXSXP")
(define-foreign-variable R-string-type int "STRSXP")
(define-foreign-variable R-list-type int "VECSXP")

(define (R-type value)
  ((foreign-lambda int "TYPEOF" c-pointer) value))

(define (R-type? value type)
  (= (R-type value) type))

(define R-null? (cut R-type? <> R-null-type))
(define R-symbol? (cut R-type? <> R-symbol-type))
(define R-boolean? (cut R-type? <> R-boolean-type))
(define R-integer? (cut R-type? <> R-integer-type))
(define R-real? (cut R-type? <> R-real-type))
(define R-complex? (cut R-type? <> R-complex-type))
(define R-string? (cut R-type? <> R-string-type))
(define R-list? (cut R-type? <> R-list-type))

(define (R-length value)
  ((foreign-lambda*
    int
    ((SEXP value))
    "C_return(Rf_length((SEXP) value));")
   value))

;; (define (R-symbol-ref vector i)
;;   ((foreign-lambda*
;;     c-string
;;     ((SEXP vector)
;;      (int i))
;;     "C_return(CHAR(PRINTNAME((SEXP) vector)[i]));")
;;    vector
;;    i))

(define (R-boolean-ref vector i)
  ((foreign-lambda*
    bool
    ((SEXP vector)
     (int i))
    "C_return(LOGICAL((SEXP) vector)[i]);")
   vector
   i))

(define (R-integer-ref vector i)
  ((foreign-lambda*
    int
    ((SEXP vector)
     (int i))
    "C_return(INTEGER((SEXP) vector)[i]);")
   vector
   i))

(define (R-vector-ref vector i)
  ((foreign-lambda*
    SEXP
    ((SEXP vector)
     (int i))
    "C_return(VECTOR_ELT((SEXP) vector, i));")
   vector
   i))

(define (R-string-ref vector i)
  ((foreign-lambda*
    c-string
    ((SEXP vector)
     (int i))
    "C_return(CHAR(STRING_ELT((SEXP) vector, i)));")
   vector
   i))

(define (R-real-ref vector i)
  ((foreign-lambda*
    double
    ((SEXP vector)
     (int i))
    "C_return(REAL((SEXP) vector)[i]);")
   vector
   i))

(define (R-complex-ref vector i)
  (receive (real imaginary)
    ((foreign-primitive
      void
      ((SEXP vector)
       (int i))
      "Rcomplex complex = COMPLEX((SEXP) vector)[i];"
      "C_word *real = C_alloc(C_SIZEOF_FLONUM);"
      "C_word *imag = C_alloc(C_SIZEOF_FLONUM);"
      "C_values(4, C_SCHEME_UNDEFINED, C_k, C_flonum(&real, complex.r), C_flonum(&imag, complex.i));")
     vector
     i)
    (make-rectangular real imaginary)))

(define (scheme-symbol symbol)
  (string->symbol
   ((foreign-lambda*
     c-string
     ((SEXP symbol))
     "C_return(CHAR(PRINTNAME((SEXP) symbol)));")
    symbol)))

;; (trace scheme-symbol)
;; (trace R-symbol)

(define scheme-vector
  (case-lambda
   ((value ref)
    (scheme-vector value ref (R-length value)))
   ((value ref length)
    (do ((vector (make-vector length))
         (i 0 (+ i 1)))
        ((= i length) vector)
      (vector-set! vector i (ref value i))))))

(define (scheme-vector-or-scalar value ref)
  (let ((length (R-length value)))
    (if (= length 1)
        (ref value 0)
        (scheme-vector value ref length))))

(define (R->scheme value)
  #;
  (debug 'r->scheme
         value
         (R-length value)
         (R-type value))
  (type-case* value
    (R-null (void))
    (R-integer
     (scheme-vector-or-scalar it R-integer-ref))
    (R-list
     (scheme-vector it (compose R->scheme R-vector-ref)))
    (R-string
     (scheme-vector-or-scalar it R-string-ref))
    (R-real
     (scheme-vector-or-scalar it R-real-ref))
    (R-boolean
     (scheme-vector-or-scalar it R-boolean-ref))
    (R-complex
     (scheme-vector-or-scalar it R-complex-ref))
    (R-symbol
     (scheme-symbol it))
    (else value)))

;; (trace R->scheme)

(define (R-function name)
  ((foreign-lambda*
    SEXP
    ((symbol name))
    "C_return(Rf_findFun(Rf_install(name), R_GlobalEnv));")
   name))

(define (R-apply f args)
  @("Apply the list of arguments to a function."
    (f "Function as a string to apply")
    (args "List of arguments to apply to f")
    (@to "R-pointer"))
  (receive (named-args unnamed-args)
    (named×unnamed args)
    (let ((named-args (map (match-lambda ((name . arg)
                                     (cons name (scheme->R arg))))
                           named-args))
          (unnamed-args (map scheme->R unnamed-args))
          (f (R-function f)))
      ;; (debug named-args unnamed-args f)
      ((foreign-lambda*
        SEXP
        ((SEXP f)
         (int error)
         (scheme-object named)
         (scheme-object unnamed))
        "int nargs = C_unfix(C_fixnum_plus(C_i_length(named), C_i_length(unnamed)));"
        "SEXP expression = allocVector(LANGSXP, nargs + 1);"
        "SETCAR(expression, (SEXP) f);"
        "SEXP ei = CDR(expression);"
        "while (!C_truep(C_i_nullp(unnamed))) {"
        "  SETCAR(ei, (SEXP) C_c_pointer_or_null(C_i_car(unnamed)));"
        "  unnamed = C_i_cdr(unnamed);"
        "  ei = CDR(ei);"
        "}"
        "while (!C_truep(C_i_nullp(named))) {"
        "  SEXP arg = (SEXP) C_c_pointer_or_null(C_i_cdar(named));"
        ;; "  SET_TAG(arg, install(C_c_string(C_i_caar(named))));"
        "  SETCAR(ei, arg);"
        "  named = C_i_cdr(named);"
        "  ei = CDR(ei);"
        "}"
        "R_tryEval(expression, R_GlobalEnv, &error); "
        "SEXP val = R_tryEval(expression, R_GlobalEnv, &error);"
        "return((SEXP) val);")
       f
       0
       named-args
       unnamed-args))))

;; (trace R-apply)

(define (R-eval f . args)
  @("Apply the arguments to a function and evaluate the result."
    (f "Function as a string to apply")
    (args "Arguments to apply to f")
    (@to "Scheme-object"))
  ;; Shit; we need to distinguish between e.g. lookups and calling a
  ;; niladic function. Either we special-case niladic application, or
  ;; require parens and look out for e.g. quote.
  ;; (if (null? args)
  ;;     (scheme->R f)
  ;;     (R-apply f args))
  (R-apply f args))

(define-syntax R
  (lambda (expression rename compare)
    (let ((var (cadr expression))
          (arguments (cddr expression)))
      `(R-eval ',var ,@arguments))))

(define-syntax R*
  (lambda (expression rename compare)
    (let ((var (cadr expression))
          (arguments (cddr expression)))
      `(R->scheme (R-eval ',var ,@arguments)))))
