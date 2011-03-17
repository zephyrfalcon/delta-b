;; null.scm
;; The Null object.

;; Null is a singleton; if it already exists in the builtin namespace,
;; we return that object
(define (null-object interp)
  (find-builtin-proto interp "Null"))

;;; --- methods ---

;; Cloning Null returns the object itself.
(define (m-null-clone obj args ns interp)
  obj)

(define (m-null-as-string obj args ns interp)
  (new-string-object interp "Null"))

(define *null-methods*
  (list (list "as-string" m-null-as-string)
        (list "clone" m-null-clone)
        (list "repr" m-null-as-string) ;; !
        ))

;;; --- Null proto ---

(define make-null-proto
  (make-proto-maker 'null :default 'null))
