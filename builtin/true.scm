;; true.scm

(define (true-object interp)
  (find-builtin-proto interp "True"))

;;; --- methods ---

;; True if-true: <code>
(define (m-true-if-true obj args ns interp)
  ...)

;; True if-false: <code>
(define (m-true-if-false obj args ns interp)
  (null-object))

;; True if: <true-block> <false-block>
(define (m-true-if obj args ns interp)
  ...)

(define (m-true-not obj args ns interp)
  (false-object interp))

(define (m-true-as-string obj args ns interp)
  (new-string-object interp "True"))

(define *true-methods*
  (list (list "as-string" m-true-as-string)
        (list "repr" m-true-as-string)
        ))

;;; --- True proto ---

(define make-true-proto
  (make-proto-maker 'boolean :default #t))


