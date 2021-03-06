;; test-namespace.scm

(push! *load-path* ".")
(use gauche.test)
(use namespace)

(test-start "namespace")

(let ((ns (make-namespace #f)))
  (test* "" #f (namespace-parent ns))
  (namespace-set! ns "foo" 1)
  (namespace-set! ns "bar" 2)
  (test* "" 1 (namespace-get ns "foo"))
  (test* "" #f (namespace-get ns "bletch"))
  (test* "names in namespace (local)"
         '("bar" "foo") (sort (namespace-names-local ns) string<?))
  (test* "names in namespace (all)"
         '("bar" "foo") (sort (namespace-names-all ns) string<?))

  (let ((ns2 (make-namespace ns)))
    (namespace-set! ns2 "foo" 3)
    (namespace-set! ns2 "baz" 4)
    (test* "ns2.foo is 2" 3 (namespace-get ns2 "foo"))
    (test* "ns2.bar is 2" 2 (namespace-get ns2 "bar"))
    (test* "ns2.baz is 4" 4 (namespace-get ns2 "baz"))

    ;; "foo" can be found in ns2
    (receive (value origin)
        (namespace-get ns2 "foo")
      (test* "origin namespace is ns2" #t (eq? ns2 origin)))
    ;; "bar" is found in ns
    (receive (value origin)
        (namespace-get ns2 "bar")
      (test* "origin namespace is ns" #t (eq? ns origin)))

    (namespace-update! ns2 "foo" 5)
    (namespace-update! ns2 "bar" 6)
    (test* "ns.bar is 6" 6 (namespace-get ns "bar"))
    (test* "ns2.foo is 5" 5 (namespace-get ns2 "foo"))
    (test* "ns.foo is 1" 1 (namespace-get ns "foo"))
    )

)


(test-end)