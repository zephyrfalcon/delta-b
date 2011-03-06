;; interpreter.scm

(use srfi-1)
(use gauche.record)
(use namespace)
(use tokenizer)
(use parser)
(use pretty)
(use tools)

(load "proto-tools")
(load "builtin/object")
(load "builtin/string")
(load "builtin/integer")
(load "builtin/bmethod")

(define-record-type interpreter #t #t
  builtin-ns    ;; contains protos
  toplevel-ns   ;; user space
  )

(define (new-interpreter)
  (let* ((builtin-ns (make-namespace #f))
         (toplevel-ns (make-namespace builtin-ns)))
    (let ((interp (make-interpreter builtin-ns toplevel-ns)))
      (init-interpreter interp)
      interp)))

(define (init-interpreter interp)
  (add-protos interp)
  (add-proto-methods interp)
  interp)

(define *protos*
  (list (list "Object"        make-object-proto       *object-methods*)
        (list "Integer"       make-integer-proto      *integer-methods*)
        (list "String"        make-string-proto       *string-methods*)
        (list "BuiltinMethod" make-bmethod-proto      *bmethod-methods*)))

(define (add-protos interp)
  (let ((ns (interpreter-builtin-ns interp)))
    (for-each-pair
     (lambda (name proto-constructor)
       (namespace-set! ns name (proto-constructor interp)))
     *protos*)
    interp))

(define (add-proto-methods interp)
  (let ((bns (interpreter-builtin-ns interp)))
    (for-each
     (lambda (proto-entry)
       (let ((proto (namespace-get bns (first proto-entry))))
         (add-proto-methods-1 interp proto (third proto-entry))))
     *protos*)))

(define (find-builtin-proto interp name)
  (let ((ns (interpreter-builtin-ns interp)))
    (namespace-get ns name)))

;; Tokenize, parse and evaluate the Delta code in the given string.
;; XXX do we need to pass a namespace?
(define (delta-eval-string s interp)
  (let* ((tokens (tokenize s))
         (_ (when *debug* (printf "[tokens] ~s~%" tokens)))
         (stmts (match-program tokens))
         (ns (interpreter-toplevel-ns interp)))
    (when *debug*
      (pretty-print stmts))
    (let ((result #f))
      (for-each
       (lambda (expr)
         (set! result (delta-eval expr ns interp)))
       stmts)
      result)))
;; FIXME: printing should be done by the caller
;; and we should probably pass _all_ of the results, not just the last...

;; Get the representation of a Delta object by calling its 'repr'
;; method. Returns a Delta object (presumably a String).
(define (get-delta-object-repr obj ns interp)
  (let* ((method (delta-object-get-slot obj "repr"))
         (f (delta-object-data method)))
    (f obj '() ns interp)))

;; Evaluate the Delta expression EXPR (an AST object) in namespace NS.
(define (delta-eval expr ns interp)
  (cond ((ast-literal? expr)
         (case (second expr)
           ((integer) (new-integer-object interp (third expr)))
           ((float) ...)
           ((string) (new-string-object interp (third expr)))
           ((symbol) ...)
           ((identifier) ...)
           (else ...)))
        ((ast-block? expr)
         ...)
        ((ast-method-call-chain? expr)
         (delta-eval-mcc expr ns interp))
        (else (error "Unknown AST node type:" expr))))

;; Evaluate a method call chain.
(define (delta-eval-mcc mcc ns interp)
  (let ((head (ast-method-call-chain-head mcc))
        (calls (ast-method-call-chain-calls mcc)))
    (let* ((target (delta-eval head ns interp))
           (result target))
      (for-each (lambda (mc)
                  (let ((method-name (ast-method-call-method mc))
                        (args (ast-method-call-args mc)))
                    (let ((value (delta-eval-method-call result
                                                         method-name args
                                                         ns interp)))
                      (set! result value))))
                calls)
      result)))

;; XXX for now, assumes it's a built-in method, i.e. we get an
;; instance of BuiltinMethod with an actual built-in Scheme function
;; associated with it. add other possibilities later.
;; maybe add a general mechanism that looks for a 'call' method...
(define (delta-eval-method-call target method-name ast-args ns interp)
  (let ((method (delta-object-get-slot target method-name)))
    (if method
        (let ((f (delta-object-data method))
              (evaled-args (map (^a (delta-eval a ns interp)) ast-args)))
          (f target evaled-args ns interp))
        (error "Unknown method:" method-name))))
