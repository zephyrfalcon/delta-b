;; builtin/string.scm

(define (make-string-proto interp)
  (let ((obj (new-delta-object :data "" :type-tag 'string)))
    ;; XXX add stuff here...
    obj))

;;; --- methods ---

(define (m-string-length obj args ns interp)
  (let* ((s (delta-object-data obj))
         (len (string-length s)))
    (new-integer-object interp len))) 

(define *string-methods*
  (list (list "length" m-string-length)
        ))