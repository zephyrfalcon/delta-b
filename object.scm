;; object.scm

(use gauche.record)

(define-record-type delta-object #t #t
  (protos)   ;; a list of protos
  (data)     ;; data for (partially) built-in objects; default #f 
  (type-tag) ;; indicates type of built-in objects; for internal use
  (id)       ;; integer
  (slots)    ;; a hash table
  )

;; Printable representation of a DELTA-OBJECT record. Mostly for
;; testing purposes.
;; XXX maybe there's a standard way to do this? Chicken has one...
(define (delta-object-repr obj)
  (format "#(delta-object :protos ~s :data ~s :type-tag ~s :id ~s :slots ~s)"
          (delta-object-protos obj)
          (delta-object-data obj)
          (delta-object-type-tag obj)
          (delta-object-id obj)
          (hash-table-keys (delta-object-slots obj))))

;; Generate a new ID for a Delta object. Not thread-safe, but that
;; doesn't matter right now. :-)
(define generate-id
  (let ((counter 0))
    (lambda ()
      (inc! counter)
      counter)))

(define (new-delta-object :key (data #f) (type-tag 'none))
  (make-delta-object '() data type-tag (generate-id)
                     (make-hash-table 'string=?)))

;; default cloning mechanism.
(define (clone-object obj :key (data #f))
  (let ((new-obj (new-delta-object)))
    (delta-object-protos-set! new-obj (list obj))
    (delta-object-data-set! new-obj (or data (delta-object-data obj)))
    (delta-object-type-tag-set! new-obj (delta-object-type-tag obj))
    new-obj))

(define (delta-object-add-slot! obj slot-name value)
  (let ((slots (delta-object-slots obj)))
    (hash-table-put! slots slot-name value)))

(define (delta-object-append-proto! obj proto)
  ...)

(define (delta-object-prepend-proto! obj proto)
  ...)

(define (delta-object-get-slot obj slot-name)
  (let ((value (hash-table-get (delta-object-slots obj) slot-name #f)))
    (if value
        (values value obj) ;; the value, and the object containing it
        (call/cc (lambda (return)
                   (for-each
                    (lambda (proto)
                      (receive (value the-obj)
                          (delta-object-get-slot proto slot-name)
                        (when value
                          (return value the-obj))))
                    (delta-object-protos obj))
                   (return #f #f))))))

(define (delta-object-mro obj)
  ...)

(define (delta-object-update-slot! obj slot-name value)
  ...)

(define (delta-object-slotnames obj)
  ...)

(define (delta-object-all-slotnames obj)
  ...)
