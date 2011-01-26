;; builtin/integer.scm

(define (make-integer-proto interp)
  (let* ((bns (interpreter-builtin-ns interp))
         (obj-proto (namespace-get bns "Object"))
         (int-proto (clone-object obj-proto)))
    (delta-object-data-set! int-proto 0)
    (delta-object-type-tag-set! int-proto 'integer)
    ;; XXX add stuff here...
    int-proto))

;; create a new Delta Integer object from a Scheme integer.
(define (new-integer-object interp n)
  (let ((int-proto (find-builtin-proto interp "Integer"))
        (obj (clone-object int-proto :data n)))
    obj))

;;; --- methods ---

