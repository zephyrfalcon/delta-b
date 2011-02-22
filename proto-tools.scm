;; proto-tools.scm

(define (make-proto-maker type-tag methods :key (default #f) (parent "Object"))
  (lambda (interp)
    (let* ((bns (interpreter-builtin-ns interp))
           (obj-proto (namespace-get bns parent))
           (this-proto (clone-object obj-proto)))

      ;; add value and type tag
      (delta-object-data-set! this-proto default)
      (delta-object-type-tag-set! this-proto type-tag)

      ;; add methods
      (for-each
       (lambda (pair)
         (let ((method-name (first pair))
               (method (second pair)))
           (delta-object-add-slot! this-proto method-name method)))
       methods)
      
      this-proto)))
