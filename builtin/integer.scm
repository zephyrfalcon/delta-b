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

(define (m-integer-as-string obj args ns interp)
  (new-string-object interp (format "~s" (delta-object-data obj))))
(define m-integer-repr m-integer-as-string)

(define *integer-methods*
  (list (list "as-string" m-integer-as-string)
        (list "plus" m-integer-plus)
        (list "repr" m-integer-repr)
        ))

;;; --- Integer prototype ---

(define make-integer-proto
  (make-proto-maker 'integer :default 0))
