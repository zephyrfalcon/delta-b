;; builtin/symbol.scm

;; create a new Delta Symbol from a Scheme string or symbol.
(define (new-symbol-object interp sym)
  (let* ((symbol-proto (find-builtin-proto interp "Symbol"))
         (value (cond ((string? sym) sym)
                      ((symbol? sym) (symbol->string sym))
                      (else (error "string or symbol required " sym))))
         (obj (clone-object symbol-proto :data value)))
    obj))
;; XXX use some kind of cache

;;; --- methods ---

(define (m-symbol-repr obj args ns interp)
  (new-string-object interp (string-append "#" (delta-object-data obj))))

(define (m-symbol-as-string obj args ns interp)
  (new-string-object interp (delta-object-data obj)))

(define *symbol-methods*
  (list (list "as-string" m-symbol-as-string)
        (list "repr" m-symbol-repr)))

;;; --- Symbol prototype ---

(define make-symbol-proto
  (make-proto-maker 'symbol :default "xyzzy"))

;; Is there such a thing as an empty Symbol? Hmm...


