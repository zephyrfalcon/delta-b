;; pretty.scm
;; Simple pretty-printer for s-expressions. Used for debugging.
;; Source: http://practical-scheme.net/wiliki/wiliki.cgi?Gauche%3APrettyPrint

(define-module pretty
  (export pretty-print)

  (define (pretty-print s)
    (define (do-indent level)
      (dotimes (_ level) (write-char #\space)))
    (define (pp-parenl)
      (write-char #\())
    (define (pp-parenr)
      (write-char #\)))
    (define (pp-atom e prefix)
      (when prefix (write-char #\space))
      (write e))
    (define (pp-list s level prefix)
      (and prefix (do-indent level))
      (pp-parenl)
      (let loop ((s s)
                 (prefix #f))
        (if (null? s)
            (pp-parenr)
            (let1 e (car s)
                  (if (list? e)
                      (begin (and prefix (newline))
                             (pp-list e (+ level 1) prefix))
                      (pp-atom e prefix))
                  (loop (cdr s) #t)))))
    (if (list? s)
        (pp-list s 0 #f)
        (write s))
    (newline))

  ) ;; end of module

