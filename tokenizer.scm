;; tokenizer.scm

(use srfi-13)

;;; --- regular expressions ---

(define re-int #/^(-?[0-9]+)/)
(define re-float #/^(-?[0-9]+\.[0-9]+)/)
(define re-string #/^(\"[^\"]*\")/)

(define re-id #/^([a-zA-Z~][a-zA-Z0-9-~]*)/)
(define re-method-call
  (string->regexp (string-append (regexp->string re-id) ":")))
(define re-symbol
  (string->regexp (string-append "^#" (string-drop (regexp->string re-id) 1))))

(define re-dot #/\./)
(define re-lparen #/\(/)
(define re-rparen #/\)/)
(define re-lbrace #/\{/)
(define re-rbrace #/\}/)
(define re-whitespace #/\s+/)

;;; --- matching procedures ---

(define (make-regex-matcher regex token-name transformer)
  (lambda (s)
    (let ((match (rxmatch regex s)))
      (if match
          (let ((matched (rxmatch-substring match)))
            (values (list token-name (transformer matched))
                    (substring s (string-length matched) (string-length s))))
          #f))))

(define match-int
  (make-regex-matcher re-int 'integer string->number))
(define match-float
  (make-regex-matcher re-float 'float string->number))
(define match-string
  (make-regex-matcher re-string 'string identity)) ;; FIXME
(define match-method-call
  (make-regex-matcher re-method-call 'method-call identity))
(define match-identifier
  (make-regex-matcher re-id 'identifier identity))
(define match-symbol
  (make-regex-matcher re-symbol 'symbol identity))

(define match-dot (make-regex-matcher re-dot 'dot identity))
(define match-lparen (make-regex-matcher re-lparen 'lparen identity))
(define match-rparen (make-regex-matcher re-rparen 'rparen identity))
(define match-lbrace (make-regex-matcher re-lbrace 'lbrace identity))
(define match-rbrace (make-regex-matcher re-rbrace 'rbrace identity))

(define (match-error s)
  (error "Invalid syntax: " s))

(define *matchers*
  (list match-int
        match-float
        match-string
        match-method-call
        match-identifier
        match-symbol
        match-dot
        match-lparen
        match-rparen
        match-lbrace
        match-rbrace
        match-error))

(define (find-next-token s)
  ...)

(define (tokenize s)
  ...)
