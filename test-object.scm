;; test-object.scm

(push! *load-path* ".")
(use gauche.test)
(load "delta-object")

(test-start "delta-object")

(let ((obj (new-delta-object)))
  (delta-object-add-slot! obj "foo" 42)
  (delta-object-add-slot! obj "bar" "blah")

  (test* "obj.foo is 42"
         42 (delta-object-get-slot obj "foo"))

  (let ((bobj (clone-object obj)))
    (delta-object-add-slot! bobj "baz" 33)
    (delta-object-add-slot! bobj "foo" 99)

    (test* "bobj.foo is 99" 99 (delta-object-get-slot bobj "foo"))
    (test* "bobj.bar is obj.bar" "blah" (delta-object-get-slot bobj "bar"))))

(test-end)
