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

;; XXX handle escape chars etc?
(define (m-string-repr obj args ns interp)
  (let* ((s (delta-object-data obj))
         (repr (format "~s" s)))
    (new-string-object interp s)))

(define (m-string-as-string obj args ns interp)
  (new-string-object interp (delta-object-data obj)))

(define *string-methods*
  (list (list "as-string" m-string-as-string)
        (list "length" m-string-length)
        (list "repr" m-string-repr)
        ))

;;; --- String prototype ---

(define make-string-proto
  (make-proto-maker 'string *string-methods* :default ""))
