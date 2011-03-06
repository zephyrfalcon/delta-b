;; builtin/object.scm

;;; --- methods ---

(define (m-object-id obj args ns interp)
  (let* ((id (delta-object-id obj))
         (int-obj (new-integer-object interp id)))
    int-obj))

(define (m-object-repr obj args ns interp)
  (new-string-object (delta-object-repr obj)))
(define m-object-as-string m-object-repr)

(define (m-object-set-slot obj args ns interp)
  ...)

;; Object get: #name [default]
(define (m-object-get-slot obj args ns interp)
  (let ((slot-name (first args))
        (default (list-ref args 1 #f)))
    (if default
        (or (delta-object-get-slot obj slot-name) default) ;; use default 
        (delta-object-get-slot obj slot-name))))           ;; may raise error

(define (m-object-update-slot obj args ns interp)
  ...)

(define *object-methods*
  (list (list "as-string" m-object-as-string)
        (list "get-slot" m-object-get-slot)
        (list "id" m-object-id)
        (list "repr" m-object-repr)
        ))

;;; --- Object prototype ---

(define make-object-proto
  (make-proto-maker #f *object-methods* :parent #f))
