;; test-object.scm

(push! *load-path* ".")
(use gauche.test)
(load "object")

(test-start "object")

(let ((obj (new-delta-object)))
  (delta-object-add-slot! obj "foo" 42)
  (delta-object-add-slot! obj "bar" "blah")

  (test* "obj.foo is 42"
         42 (delta-object-get-slot obj "foo"))

  #t)

(test-end)
