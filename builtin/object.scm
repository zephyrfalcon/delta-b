;; builtin/object.scm

;;; --- methods ---

(define (m-object-id obj args ns interp)
  (let* ((id (delta-object-id obj))
         (int-obj (make-integer-obj interp id)))
    int-obj))

(define *object-methods*
  (list (list "id" m-object-id)
        ))

;;; --- Object prototype ---

(define make-object-proto
  (make-proto-maker #f *object-methods* :parent #f))
