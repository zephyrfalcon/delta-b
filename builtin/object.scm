;; builtin/object.scm

;;; --- methods ---

(define (m-object-id obj args ns interp)
  (let* ((id (delta-object-id obj))
         (int-obj (new-integer-object interp id)))
    int-obj))

(define (m-object-repr obj args ns interp)
  (new-string-object (delta-object-repr obj)))
(define m-object-as-string m-object-repr)

(define *object-methods*
  (list (list "as-string" m-object-as-string)
        (list "id" m-object-id)
        (list "repr" m-object-repr)
        ))

;;; --- Object prototype ---

(define make-object-proto
  (make-proto-maker #f *object-methods* :parent #f))
