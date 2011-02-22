;; builtin/integer.scm

;; create a new Delta Integer object from a Scheme integer.
(define (new-integer-object interp n)
  (let* ((int-proto (find-builtin-proto interp "Integer"))
         (obj (clone-object int-proto :data n)))
    obj))

;;; --- methods ---

(define *integer-methods* '())

;;; --- Integer prototype ---

(define make-integer-proto
  (make-proto-maker 'integer *integer-methods* :default 0))