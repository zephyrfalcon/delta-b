;; builtin/integer.scm

;; create a new Delta Integer object from a Scheme integer.
(define (new-integer-object interp n)
  (let* ((int-proto (find-builtin-proto interp "Integer"))
         (obj (clone-object int-proto :data n)))
    obj))

;;; --- methods ---

(define (m-integer-plus obj args ns interp)
  (let ((sum (+ (delta-object-data obj)
                (delta-object-data (car args)))))
    (new-integer-object interp sum)))

(define *integer-methods*
  (list (list "plus" m-integer-plus)
        ))

;;; --- Integer prototype ---

(define make-integer-proto
  (make-proto-maker 'integer *integer-methods* :default 0))