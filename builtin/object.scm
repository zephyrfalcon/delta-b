;; builtin/object.scm

(define (make-object-proto interp)
  (let ((obj (new-delta-object)))
    ;; XXX add stuff here...
    obj))

;;; --- methods ---

(define (m-object-id obj args ns interp)
  (let* ((id (delta-object-id obj))
         (int-obj (make-integer-obj interp id)))
    int-obj))

(define *object-methods*
  (list (list "id" m-object-id)
        ))
