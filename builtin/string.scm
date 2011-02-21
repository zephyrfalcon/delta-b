;; builtin/string.scm
;; String proto.

(define (new-string-object interp s)
  (let* ((string-proto (find-builtin-proto interp "String"))
         (obj (clone-object string-proto :data s)))
    obj))

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
        (list "repr" m-string-repr)
        ))

;;; --- String prototype ---

(define make-string-proto
  (make-proto-maker 'string *string-methods* :default ""))
