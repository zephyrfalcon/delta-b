;; builtin/string.scm

(define-module builtin.string
  (export make-string-proto))
(select-module builtin.string)

(use object)

(define (make-string-proto)
  (let ((obj (new-delta-object :data "" :type-tag 'string)))
    ;; XXX add stuff here...
    obj))

;;; --- methods ---

(define (delta-string-length obj)
  (let* ((s (delta-object-data obj))
         (len (string-length s)))
    (%new-delta-number len))) ;; FIXME

(define *method-table*
  (list (list "length" delta-string-length)
        ))