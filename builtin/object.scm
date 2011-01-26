;; builtin/object.scm

(define (make-object-proto interp)
  (let ((obj (new-delta-object)))
    ;; XXX add stuff here...
    obj))

;;; --- methods ---

(define (_delta-object-id interp obj)
  (let* ((id (delta-object-id obj))
         (int-obj (make-integer-obj interp id)))
    int-obj))

(define *method-table*
  (list (list "id" _delta-object-id)
        ))
