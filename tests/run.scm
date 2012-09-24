(use numbers R test)

(test "Pi via named arguments to get"
      3.14159265358979
      (R-eval "get" x: "pi"))

(test "Scalar integer"
      2
      (R-eval "c" 2))

(test "Vector integers"
      '#(2 3)
      (R-eval "c" 2 3))

(test "Nested vector-integers"
      '#(2 3 4 5)
      (R-eval "c" 2 3 (R-eval "c" 4 5)))

(test "Nested list-integers"
      '#(2 3 #(4 5))
      (R-eval "list" 2 3 (R-eval "list" 4 5)))

(test "String scalar"
      "harro"
      (R-eval "c" "harro"))

(test "String vector"
      '#("harro" "harro")
      (R-eval "rep" "harro" 2))

(test "Scalar real"
      2.1
      (R-eval "c" 2.1))

(test "Scalar vector"
      '#(2.1 2.1)
      (R-eval "rep" 2.1 2))

(test "Scalar boolean: false"
      #f
      (R-eval "c" #f))

(test "Scalar boolean: true"
      #t
      (R-eval "c" #t))

(test "Vector boolean"
      '#(#f #f)
      (R-eval "rep" #f 2))
;; Doesn't respect exactness; i.e., complex numbers are represented
;; by doubles in R.
(test "Scalar complex"
      (make-rectangular 3.0 3.0)
      (R-eval "c" (make-rectangular 3 3)))

(test "Vector complex"
      (make-vector 2 (make-rectangular 3.0 3.0))
      (R-eval "rep" (make-rectangular 3 3) 2))

(let ((env (R-eval "new.env")))
  (R-eval "assign" "a" 2 R-missing env)
  (test-assert "Opaque object"
               (R-eval "exists" "a" R-missing env)))
