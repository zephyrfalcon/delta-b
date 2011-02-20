;; proto-tools.scm

(define (make-proto-maker type-tag :key (default #f) (parent "Object"))
  (lambda (interp)
    (let* ((bns (interpreter-builtin-ns interp))
           (obj-proto (namespace-get bns parent))
           (this-proto (clone-object obj-proto)))
      (delta-object-data-set! this-proto default)
      (delta-object-type-tag-set! this-proto type-tag)
      ;; XXX add methods here...
      this-proto)))
