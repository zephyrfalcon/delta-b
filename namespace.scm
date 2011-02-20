;; namespace.scm

(define-module namespace
  (export-all))
(select-module namespace)

;; a namespace is represented by a pair (parent . slots).
(define (make-namespace parent)
  (cons parent (make-hash-table 'string=?)))
(define (namespace-parent ns) (car ns))
(define (namespace-slots ns) (cdr ns))

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

;; look for the name in the namespace and its parents, then rebind
;; that name in the namespace where it was found. returns #t if found
;; and replaced, #f otherwise.
(define (namespace-update! ns name value)
  (receive (old-value in-ns)
      (namespace-get ns name)
    (if old-value
        (begin
          (namespace-set! in-ns name value)
          #t)
        #f)))

;; delete the name from the namespace (locally, i.e. parent namespaces
;; are unaffected even if the name exists there). returns #t if found
;; and deleted, #f otherwise.
(define (namespace-delete! ns name)
  (hash-table-delete! (namespace-slots ns) name))

;; return a list of all the names defined in the namespace. order is undefined.
(define (namespace-names-local ns)
  (hash-table-keys (namespace-slots ns)))

(define (namespace-names-all ns)
  (let* ((names (namespace-names-local ns))
         (parent (namespace-parent ns)))
    (if parent
        (append names (namespace-names-all parent))
        names)))


