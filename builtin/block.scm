;; builtin/block.scm
;; Block proto/record.

;; A block consists of a list of Delta expressions, and a namespace
;; (usually the namespace in which the block was created).

(define-record-type delta-block #t #t
  exprs
  namespace)

(define (make-delta-block-from-ast ast-block ns)
  (make-delta-block (ast-block-statements ast-block) ns))

;; Create a Delta object from a Scheme delta-block record.
(define (new-block-object interp block)
  (let* ((block-proto (find-builtin-proto interp "Block"))
         (obj (clone-object block-proto :data block)))
    obj))

;;; --- methods ---

(define *block-methods* '())

;;; --- Block proto ---

(define dummy-block (make-delta-block '() #f))

(define make-block-proto
  (make-proto-maker 'block *block-methods* :default dummy-block))
