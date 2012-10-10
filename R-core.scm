@(title "R")
@(description "R interface for Chicken Scheme")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(username "klutometis")

@(heading "Overview")

@(text "{{R}} provides a simple way to call R-functions, consisting of the following two forms: {{R}} and {{R*}}. {{R}} evaluates an R-expression, returning an opaque R-pointer that can be passed to other R-functions. Use this, for instance, when you don't need to modify the object in Scheme.
")

@(text "{{R*}}, on the other hand, evaluates the expression and tries to translate it into Scheme; it understands {{NULL}}, lists, strings, reals, bools, complex numbers and symbols. Everything else is opaque.")

@(heading "Documentation")
(foreign-declare
 #<<END
 #include <Rembedded.h>
 #include <Rinternals.h>
 #include <R_ext/Arith.h>
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

(define-record-and-printer NA)
(define NA (make-NA))
(define (R-NA? sexp)
  ((foreign-lambda*
    bool
    ((SEXP sexp))
    "int error;"
    "C_return(Rf_asLogical(R_tryEval(lang2(install(\"is.na\"), (SEXP) sexp), R_GlobalEnv, &error)));")
   sexp))

(define (R-NaN? sexp)
  ((foreign-lambda*
    bool
    ((SEXP sexp))
    "int error;"
    "C_return(Rf_asLogical(R_tryEval(lang2(install(\"is.nan\"), (SEXP) sexp), R_GlobalEnv, &error)));")
   sexp))

(define R-missing
  ;; @("R-constant for missing arguments")
  (foreign-value "R_MissingArg" SEXP))

(define R-null
  ;; @("NULL")
  (foreign-value "R_NilValue" SEXP))

(define R-NA
  ((foreign-lambda*
    SEXP
    ()
    "C_return(ScalarLogical(NA_LOGICAL));")))

(define R-inf+
  ((foreign-lambda*
    SEXP
    ()
    "C_return(ScalarReal(R_PosInf));")))

(define R-inf-
  ((foreign-lambda*
    SEXP
    ()
    "C_return(ScalarReal(R_NegInf));")))

(define R-NaN
  ((foreign-lambda*
    SEXP
    ()
    "C_return(ScalarReal(R_NaN));")))

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
  (if (infinite? integer)
      (if (positive? integer)
          R-inf+
          R-inf-)
      ((foreign-lambda*
        SEXP
        ((int integer))
        "C_return(ScalarInteger(integer));")
       (inexact->exact integer))))

(define (R-real real)
  (if (nan? real)
      R-NaN
      ((foreign-lambda*
        SEXP
        ((double real))
        "C_return(ScalarReal(real));")
       real)))

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
  (R-eval `(c ,@(vector->list vector))))

(define (R-list list)
  (R-eval `(list ,@list)))

(define (sexp-pointer? object)
  (tagged-pointer? object 'sexp))

(define (scheme->R value)
  (type-case* value
    (NA R-NA)
    (null R-null)
    (symbol (R-symbol it))
    (boolean (R-boolean it))
    ;; +inf.0 and -inf.0 are integers.
    (integer (R-integer it))
    ;; +nan.0 and -nan.0 are reals.
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

(define scheme-vector
  (case-lambda
   ((value ref)
    (scheme-vector value ref (R-length value)))
   ((value ref length)
    (do ((vector (make-vector length))
         (i 0 (+ i 1)))
        ((= i length) vector)
      (vector-set! vector i (ref value i))))))

(define (scheme-list value ref)
  (vector->list (scheme-vector value ref)))

(define (scheme-vector-or-scalar value ref)
  (let ((length (R-length value)))
    (if (= length 1)
        (ref value 0)
        (scheme-vector value ref length))))

(define (R->scheme value)
  (type-case* value
    (R-NA NA)
    (R-null (void))
    (R-integer
     (scheme-vector-or-scalar it R-integer-ref))
    (R-list
     (scheme-list it (compose R->scheme R-vector-ref)))
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

(define (R-unbound? variable)
  ((foreign-lambda*
    bool
    ((SEXP variable))
    "C_return((SEXP) variable == R_UnboundValue);")
   variable))

(define (R-function name)
  (let ((function
         ((foreign-lambda*
           SEXP
           ((symbol name))
           "SEXP f;"
           "PROTECT(f = Rf_findVar(Rf_install(name), R_GlobalEnv));"
           "if (TYPEOF(f) == PROMSXP) {"
           "  PROTECT(f);"
           "  f = eval(f, R_GlobalEnv);"
           "  UNPROTECT(1);"
           "}"
           "switch (TYPEOF(f)) {"
           "case CLOSXP:"
           "case BUILTINSXP:"
           "case SPECIALSXP:"
           "  UNPROTECT(1);"
           "  C_return(f);"
           "default:"
           "  UNPROTECT(1);"
           "  C_return(R_UnboundValue);"
           "}")
          name)))
    (if (R-unbound? function)
        (error "Unbound R-function" name)
        function)))

(define (R-variable name)
  (let ((variable
         ((foreign-lambda*
           SEXP
           ((symbol name))
           "C_return(Rf_findVar(Rf_install(name), R_GlobalEnv));")
          name)))
    (if (R-unbound? variable)
        ;; (error "Unbound R-variable" name)
        (R-symbol name)
        variable)
    ))

(define (R-apply f args)
  ;; @("Apply the list of arguments to a function."
  ;;   (f "Function as a string to apply")
  ;;   (args "List of arguments to apply to f")
  ;;   (@to "R-pointer"))
  ;; (debug 'R-apply f args)
  (receive (named-args unnamed-args)
    (named×unnamed args)
    (let ((named-args (map (match-lambda ((name . arg)
                                     (cons name arg)))
                           named-args)))
      (receive (value err)
        ((foreign-primitive
          void
          ((SEXP f)
           (scheme-object tag)
           (scheme-object named)
           (scheme-object unnamed))
          "int nargs = C_unfix(C_fixnum_plus(C_i_length(named), C_i_length(unnamed)));"
          "SEXP expression, ei;"
          "expression = allocVector(LANGSXP, nargs + 1);"
          "SETCAR(expression, (SEXP) f);"
          "PROTECT(ei = CDR(expression));"
          "while (!C_truep(C_i_nullp(unnamed))) {"
          "  SETCAR(ei, (SEXP) C_c_pointer_or_null(C_i_car(unnamed)));"
          "  unnamed = C_i_cdr(unnamed);"
          "  ei = CDR(ei);"
          "}"
          "while (!C_truep(C_i_nullp(named))) {"
          "  SEXP arg;"
          "  PROTECT(arg = (SEXP) C_c_pointer_or_null(C_i_cdar(named)));"
          ;; Why the fuck do we have to null-terminate the string here?
          ;; Should we do a copy instead?
          "  char *name = C_c_string(C_i_caar(named));"
          "  name[C_unfix(C_i_string_length(C_i_caar(named)))] = '\\0';"
          "  SET_TAG(ei, install(name));"
          "  SETCAR(ei, arg);"
          "  named = C_i_cdr(named);"
          "  ei = CDR(ei);"
          "  UNPROTECT(1);"
          "}"
          "UNPROTECT(1);"
          "int error = 0;"
          "SEXP val = R_tryEval(expression, R_GlobalEnv, &error);"
          "C_word *pointer = C_alloc(C_SIZEOF_TAGGED_POINTER);"
          "C_values(4, C_SCHEME_UNDEFINED, C_k, C_taggedmpointer(&pointer, tag, val), C_fix(error));")
         f
         'sexp
         named-args
         unnamed-args)
        (if (zero? err)
            value
            (error "R-error (vide supra) -- R-apply"))))))

(define (quotation? expression)
  (or (eq? (car expression) 'quote)
      (eq? (car expression) 'quasiquote)))

(define (R-eval expression)
  ;; @("Evaluate the R-expression."
  ;;   (expression "The expression to evaluate")
  ;;   (@to "R-object"))
  ;; (debug expression)
  (cond ((pair? expression)
         (if (quotation? expression)
             (scheme->R (cdr expression))
             (R-apply (R-function (car expression))
                      (map R-eval (cdr expression)))))
        ((keyword? expression) expression)
        ((symbol? expression) (R-variable expression))
        (else (scheme->R expression))))

(define-syntax R
  @("Evaluate an R-expression; for instance, {{(R (rnorm 2)) => #<tagged pointer sexp 1f40818>}}."
    (expression "The expression to evaluate")
    (@to "R-object"))
  (lambda (expression rename compare)
    (list 'apply 'R-eval (list 'quasiquote (cdr expression)))))

(define-syntax R*
  @("Evaluate an R-expression and translate it into Scheme;
for instance, {{(R* (rnorm 2)) => #(-0.0740060993626383
-1.77269881184448)}}."
    (expression "The expression to evaluate")
    (@to "Scheme-object"))
  (lambda (expression rename compare)
    `(R->scheme (R ,@(cdr expression)))))
