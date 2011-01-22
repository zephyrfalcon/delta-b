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
  ...)

(define (namespace-get ns name)
  ...)

(define (namespace-get-local ns name)
  ...)

(define (namespace-get/default ns name default)
  ...)

(define (namespace-update! ns name value)
  ...)

(define (namespace-delete! ns name)
  ...)

(define (namespace-names-local ns)
  ...)

(define (namespace-names-all ns)
  ...)


