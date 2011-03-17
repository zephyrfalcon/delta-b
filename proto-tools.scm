;; proto-tools.scm

(define (make-proto-maker type-tag :key (default #f) (parent "Object"))
  (lambda (interp)
    (let* ((bns (interpreter-builtin-ns interp))
           (obj-proto (if parent (namespace-get bns parent) #f))
           (this-proto (if parent
                           (clone-object obj-proto)
                           (new-delta-object))))

      ;; add value and type tag
      (when parent
        (delta-object-data-set! this-proto default))
      (delta-object-type-tag-set! this-proto type-tag)

      ;; at this point we DON'T add methods yet, because they need to
      ;; be wrapped in BuiltinMethod, which may not exist yet.
      
      this-proto)))

(define (add-proto-methods-1 interp obj methods)
  (for-each-pair
   (lambda (method-name method)
     (let ((mobj (new-bmethod-object interp method)))
       (delta-object-add-slot! obj method-name mobj)))
   methods))
