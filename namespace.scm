;; namespace.scm

(define-module namespace
  (export-all))
(select-module namespace)

;; a namespace is represented by a pair (parent . slots).
(define (make-namespace parent)
  (cons parent (make-hash-table string=?)))
(define (namespace-parent ns) (car ns))
(define (namespace-slots) (cdr ns))

(define (namespace-set! ns name value)
  (hash-table-put! (namespace-slots ns) name value))

;; looks for the given name in the namespace and its parents. if
;; found, returns two values (value ns), where ns is the namespace
;; (possibly a parent) in which the name was bound. otherwise, returns
;; values (#f #f).
(define (namespace-get ns name)
  (let ((value (namespace-get-local ns name)))
    (if value
        (values value ns)
        (let ((parent (namespace-parent ns)))
          (if parent
              (namespace-get parent name)
              (values #f #f))))))

;; looks for the given name in the local namespace, not checking any
;; parents. if found, returns the object, otherwise #f.
(define (namespace-get-local ns name)
  (hash-table-get (namespace-slots ns) name #f))

(define (namespace-update! ns name value)
  ...)

(define (namespace-delete! ns name)
  ...)

(define (namespace-names-local ns)
  ...)

(define (namespace-names-all ns)
  ...)


