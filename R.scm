(module R
;;   @("The R-module provides functions for applying and evaluating
;; R-expressions.")
  (NA
   NA?
   R
   R*
   R->scheme
   R-eval
   R-missing
   R-null)

  (import chicken
          foreign
          scheme
          srfi-1
          srfi-13)

  (import-for-syntax matchable)

  (use (only aima define-record-and-printer)
       big-chicken
       call-with-environment-variables
       debug
       lolevel
       matchable
       moremacros
       numbers
       shell)

  (parameterize ((debug? #f))
    (include "R-core.scm")))
