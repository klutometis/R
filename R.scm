@(title "R")
@(description "R interface for Chicken Scheme")
@(author "Peter Danenberg")
@(email "pcd@roxygen.org")
@(username "klutometis")

(module R
  @("The R-module provides functions for applying and evaluating
R-expressions.")
  (R-apply R-eval R-missing)

  (import chicken
          foreign
          scheme
          srfi-1)

  (use big-chicken
       debug
       lolevel
       matchable
       moremacros
       numbers)

  (parameterize ((debug? #f))
    (include "R-core.scm")))
