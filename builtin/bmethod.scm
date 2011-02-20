;; builtin/bmethod.scm
;; Proto for built-in methods.

(define (dummy-bmethod obj args ns interp)
  #f) ;; FIXME: should return Null or something

(define (make-bmethod-proto interp)
  (let* ((bns (interpreter-builtin-ns interp))
         (obj-proto (namespace-get bns "Object"))
         (bmethod-proto (clone-object obj-proto)))
    (delta-object-data-set! bmethod-proto dummy-bmethod)
    (delta-object-type-tag-set! bmethod-proto 'bmethod)
    ;; XXX add stuff here...
    bmethod-proto))

;;; --- methods ---

(define *bmethod-methods* '())
