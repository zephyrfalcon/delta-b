;; tokenizer.scm

(define-module tokenizer
  (export tokenize))

(select-module tokenizer)

(use srfi-1)
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
          (values #f s)))))

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

;; using regexen doesn't work well here
(define (match-comment s)
  (if (string-prefix? "--" s)
      (let ((next-newline-pos (string-index s #\newline)))
        (if next-newline-pos
            (values (list 'comment (substring s 0 next-newline-pos))
                    (substring s next-newline-pos (string-length s)))
            (values (list 'comment s) "")))  ;; comment up to end of string
      (values #f #f)))

(define match-dot (make-regex-matcher re-dot 'dot identity))
(define match-lparen (make-regex-matcher re-lparen 'lparen identity))
(define match-rparen (make-regex-matcher re-rparen 'rparen identity))
(define match-lbrace (make-regex-matcher re-lbrace 'lbrace identity))
(define match-rbrace (make-regex-matcher re-rbrace 'rbrace identity))

(define (match-error s)
  (error "Invalid syntax: " s))

(define *matchers*
  (list match-float
        match-int
        match-string
        match-method-call
        match-comment
        match-identifier
        match-symbol
        match-dot
        match-lparen
        match-rparen
        match-lbrace
        match-rbrace))

(define (find-next-token s)
  (let loop ((matchers *matchers*))
    (if (null? matchers)
        (error "Invalid syntax: " s)
        (receive (matched-token rest)
            ((car matchers) s)
          (if matched-token
              (values matched-token rest) ;; found a match
              (loop (cdr matchers)))))))

(define (tokenize s)
  (let loop ((text s) (tokens '()))
    (let ((text (string-trim text)))  ;; strip leading whitespace
      (if (= (string-length text) 0)  ;; no text left?
          (filter (lambda (tok) (not (equal? (car tok) 'comment)))
                  (reverse tokens))   ;; done
          (receive (matched-token rest)
              (find-next-token text)
            (if matched-token
                (loop rest (cons matched-token tokens))
                (error "this should not happen :-o")))))))
    
