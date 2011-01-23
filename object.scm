;; object.scm

(define-module object
  (export-all))
(select-module object)

(define-record-type delta-object #t #t
  protos   ;; a list of protos
  data     ;; data for (partially) built-in objects; default #f 
  type-tag ;; indicates type of built-in objects; for internal use
  id       ;; integer
  slots    ;; a hash table
  )

(define (new-delta-object)
  (make-delta-object '() #f 'none 0 (make-hash-table 'string=?)))
