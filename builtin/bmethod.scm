;; builtin/bmethod.scm
;; Proto for built-in methods.

(define (dummy-bmethod obj args ns interp)
  #f) ;; FIXME: should return Null or something

;;; --- methods ---

(define *bmethod-methods* '())

;;; --- BuiltinMethod proto ---

(define make-bmethod-proto
  (make-proto-maker 'bmethod *bmethod-methods* :default dummy-bmethod))
