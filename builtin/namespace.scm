;; builtin/namespace.scm
;; Namespace proto.

(define (new-namespace-object interp ns)
  (let* ((ns-proto (find-builtin-proto interp "Namespace"))
         (ns-obj (clone-object ns-proto :data ns)))
    ns-obj))

;;; --- methods ---

;; Namespace current:
;; Returns the current namespace.
(define (m-namespace-current obj args ns interp)
  (new-namespace-object interp ns))

;; Namespace builtin: => wrapper around built-in namespace
;; This would return a fresh Namespace object each time it's called, which
;; may be counter-intuitive. We could avoid this by using some sort of cache
;; (e.g. an attribute on the Namespace proto itself, or a global variable
;; in Scheme, if we want it hidden).
(define (m-namespace-builtin obj args ns interp)
  ...)

(define (m-namespace-toplevel obj args ns interp)
  ...)

(define (m-namespace-set obj args ns interp)
  ...)

(define (m-namespace-get obj args ns interp)
  ...)

(define (m-namespace-update obj args ns interp)
  ...)

(define (m-namespace-parent obj args ns interp)
  ...)

(define (m-namespace-names obj args ns interp)
  ...)

(define *namespace-methods*
  (list (list "current" m-namespace-current)
        ))

;;; --- Namespace proto ---

(define make-namespace-proto
  (make-proto-maker 'namespace :default (make-namespace #f)))
