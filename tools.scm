;; tools.scm

(define-module tools
  (export-all))
(select-module tools)

(define (unique lst)
  (let ((hash (make-hash-table 'equal?)))
    (for-each (lambda (x) (hash-table-put! hash x #t))
              lst)
    (hash-table-keys hash)))

(define (printf str . args)
  (display (apply format (cons str args))))

(define (for-each-pair f lst)
  (for-each
   (lambda (x)
     (f (car x) (cadr x)))
   lst))

(define (assert x :optional (msg #f))
  (unless x (error (or msg "assertion failed"))))

;; Pythonic string slicing function.
(define (string-slice s idx1 :optional (idx2 #f))
  (let* ((sl (string-length s))
         (idx2 (or idx2 sl)) ;; defaults to length of string
         (corr-idx1 (if (negative? idx1) (+ sl idx1) idx1))
         (corr-idx2 (if (negative? idx2) (+ sl idx2) idx2)))
    #;(printf "(DEBUG) ~s, ~s~%" corr-idx1 corr-idx2)
    (if (>= corr-idx1 corr-idx2)
        ""
        (substring s
                   (min (max corr-idx1 0) sl)
                   (min (max corr-idx2 0) sl)))))

;; Gauche doesn't like "strange" characters (like #xFA) in files, and
;; using with-input-from-file and port->string does not seem to work
;; if a file contains them. Therefore we write our own function that
;; reads all of a file's contents, and removes what Gauche doesn't
;; like.

;; (If there is a better way to do this, I'd like to know about it...)

;; read the complete contents of a file, and return it as one big string.
(define (read-all-from-file filename :key (blocksize 1024))
  (let ((p (open-input-file filename)))
    (let ((data (read-all-from-port p :blocksize blocksize)))
      (close-input-port p)
      data)))

(define (read-all-from-port port :key (blocksize 1024))
  (let loop ((chunks '()))
    (let ((chunk (read-block blocksize port)))
      (if (eof-object? chunk)
          ;; remove any "weird" characters
          (string-incomplete->complete (string-join (reverse chunks) "") :omit)
          (loop (cons chunk chunks))))))

;; splits the last at the last element, and returns two values, a list
;; containing all elements but the last, and a list with only the last
;; element.
(define (split-at-last lst)
  (let ((len (length lst)))
    (split-at lst (- len 1))))
