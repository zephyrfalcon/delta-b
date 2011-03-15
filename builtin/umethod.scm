;; builtin/umethod.scm
;; User-defined methods.

;; Proto is called Method for practical reasons (having to write
;; "UserDefinedMethod" all the time gets a bit old). In a more perfect
;; world, Method would be the parent object of both BuiltinMethod and
;; UserDefinedMethod. Maybe in Delta/C...

(define-record-type delta-umethod #t #t
  args    ;; list of arguments (as Scheme strings)
  block   ;; instance of delta-block; comes with its own namespace
  )

;;; --- methods ---

(define *umethod-methods* '())

;;; --- Method proto ---

(define dummy-umethod 'FIXME)

(define make-umethod-proto
  (make-proto-maker 'umethod *umethod-methods* :default dummy-umethod))
  

