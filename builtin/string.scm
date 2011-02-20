;; builtin/string.scm

(define (make-string-proto interp)
  (let ((obj (new-delta-object :data "" :type-tag 'string)))
    ;; XXX add stuff here...
    obj))

(define (new-string-object interp s)
  ...)

;;; --- methods ---

(define (m-string-length obj args ns interp)
  (let* ((s (delta-object-data obj))
         (len (string-length s)))
    (new-integer-object interp len)))

(define (m-string-repr obj args ns interp)
  (let* ((s (delta-object-data obj))
         (repr (format "~s" s)))
    (new-string-object interp s)))

(define *string-methods*
  (list (list "length" m-string-length)
        ))