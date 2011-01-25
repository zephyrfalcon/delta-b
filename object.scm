;; object.scm

(define-module object
  (export-all))
(select-module object)

(use gauche.record)

(define-record-type delta-object #t #t
  protos   ;; a list of protos
  data     ;; data for (partially) built-in objects; default #f 
  type-tag ;; indicates type of built-in objects; for internal use
  id       ;; integer
  slots    ;; a hash table
  )

(define (new-delta-object :key (data #f) (type-tag 'none))
  (make-delta-object '() data type-tag 0 (make-hash-table 'string=?)))

;; default cloning mechanism.
(define (clone-object obj :key (data #f))
  (let ((new-obj (new-delta-object)))
    (delta-object-protos-set! new-obj (list obj))
    (delta-object-data-set! new-obj (or data (delta-object-data obj)))
    (delta-object-type-tag-set! new-obj (delta-object-type-tag obj))
    new-obj))