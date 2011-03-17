;; builtin/object.scm

;;; --- methods ---

(define (m-object-id obj args ns interp)
  (let* ((id (delta-object-id obj))
         (int-obj (new-integer-object interp id)))
    int-obj))

(define (m-object-repr obj args ns interp)
  (new-string-object interp (delta-object-repr obj)))
(define m-object-as-string m-object-repr)

;; Object set-slot: #name value
;; Set a slot with the given name to the given value, on this object. If it
;; exists, it is overwritten. Does not affect any parent objects that might
;; have a slot with the same name; for that, see update-slot.
;; RETURNS: The object itself (useful for chaining).
(define (m-object-set-slot obj args ns interp)
  (let ((slot-name (delta-object-data (first args))) ;; must be a Symbol
        (value (second args)))
    (delta-object-add-slot! obj slot-name value)
    obj))

;; Object get: #name [default]
(define (m-object-get-slot obj args ns interp)
  (let ((slot-name (delta-object-data (first args))) ;; must be a Symbol
        (default (list-ref args 1 #f)))
    (if default
        (or (delta-object-get-slot obj slot-name) default) ;; use default 
        (delta-object-get-slot obj slot-name))))           ;; may raise error

(define (m-object-update-slot obj args ns interp)
  ...)

(define (m-object-local-slots obj args ns interp)
  ...)

(define (m-object-all-slots obj args ns interp)
  ...)

(define *object-methods*
  (list (list "as-string" m-object-as-string)
        (list "get-slot" m-object-get-slot)
        (list "id" m-object-id)
        (list "repr" m-object-repr)
        (list "set-slot" m-object-set-slot)
        ))

;;; --- Object prototype ---

(define make-object-proto
  (make-proto-maker #f :parent #f))
