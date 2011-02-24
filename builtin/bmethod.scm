;; builtin/bmethod.scm
;; Proto for built-in methods.

(define (dummy-bmethod obj args ns interp)
  #f) ;; FIXME: should return Null or something

;; create a new BuiltinMethod object from a Scheme function.
(define (new-bmethod-object interp f)
  (let ((bmethod-proto (find-builtin-proto interp "BuiltinMethod")))
    (clone-object bmethod-proto :data f)))

;;; --- methods ---

(define *bmethod-methods* '())

;;; --- BuiltinMethod proto ---

(define make-bmethod-proto
  (make-proto-maker 'bmethod *bmethod-methods* :default dummy-bmethod))
