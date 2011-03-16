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

(define (new-umethod-object interp args block)
  (let* ((umethod-proto (find-builtin-proto interp "Method"))
         (umethod (make-delta-umethod args block))
         (uobj (clone-object 'umethod :data umethod)))
    uobj))

;;; --- methods ---

;; Method new: [args...] block
;; Create a new user-defined method with zero or more arguments.
(define (m-umethod-new obj args ns interp)
  ...)
;; need: zero or more symbols
;; then a block (already parsed & evaluated as a Delta Block object)
;; everything else will be ignored
;; pass this to new-umethod-object

;; Method call: target [args...]
;; A new namespace will be created to evaluate the method's code, based on
;; the namespace associated with the method's block.
;; Target will be bound to "~". Args will be bound to those defined on the
;; method. For now: if not enough arguments are passed, then those not defined
;; will default to Null; OTOH, superfluous arguments will be ignored.
;; Other objects may be bound as well (e.g. maybe a shorthand to the current
;; namespace?).
(define (m-umethod-call obj args ns interp)
  ...)

(define (m-umethod-clone obj args ns interp)
  ...)

(define *umethod-methods*
  (list (list "call" m-umethod-call)
        (list "new" m-umethod-new)
        ))

;;; --- Method proto ---

(define dummy-umethod 'FIXME)

(define make-umethod-proto
  (make-proto-maker 'umethod *umethod-methods* :default dummy-umethod))
  

