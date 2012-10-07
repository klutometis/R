(use debug numbers R test)

(test "Pi via named arguments to get"
      3.14159265358979
      (R* (get x: "pi")))

(test "Scalar integer"
      2
      (R* 2))

(test "Vector integers"
      '#(2 3)
      (R* (c 2 3)))

(test "Nested vector-integers"
      '#(2 3 4 5)
      (R* (c 2 3 (c 4 5))))

(test "Nested list-integers"
      '(2 3 (4 5))
      (R* (list 2 3 (list 4 5))))

(test "String scalar"
      "harro"
      (R* "harro"))

(test "String vector"
      '#("harro" "harro")
      (R* (rep "harro" 2)))

(test "Scalar real"
      2.1
      (R* 2.1))

(test "Scalar vector"
      '#(2.1 2.1)
      (R* (rep 2.1 2)))

(test "Scalar boolean: false"
      #f
      (R* #f))

(test "Scalar boolean: true"
      #t
      (R* #t))

(test "Vector boolean"
      '#(#f #f)
      (R* (rep #f 2)))

;; Doesn't respect exactness; i.e., complex numbers are represented
;; by doubles in R.
(test "Scalar complex"
      (make-rectangular 3.0 3.0)
      (R* (c (make-rectangular 3 3))))

(test "Vector complex"
      (make-vector 2 (make-rectangular 3.0 3.0))
      (R* (rep (make-rectangular 3 3) 2)))

(test "Named arguments"
      '#(3 3)
      (R* (rep.int times: 2 x: 3)))

(let ((env (R (new.env))))
  (R (assign "a" 2 pos: env))
  (test-assert "Opaque object"
               (R* (exists "a" R-missing env))))

(test "Attributes"
      '#("a" "b")
      (R* (attr (list a: 1 b: 2) "names")))
