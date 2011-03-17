;; builtin/list.scm

;; List prototype. Because performance is not a goal for Delta/B, I'll
;; just implement it here as Scheme lists under the hood. In a more
;; realistic implementation of the language, performance would ideally
;; be more like Python's lists (i.e. O(1) to append, access or take
;; length).

(define (new-list-object interp lst)
  (let* ((list-proto (find-builtin-proto interp "List"))
         (obj (clone-object list-proto :data lst)))
    obj))

;;; --- methods ---

(define (m-list-length obj args ns interp)
  (new-integer-object interp (length (delta-object-data obj))))

;; List append: [...args...]
(define (m-list-append obj args ns interp)
  (let ((new-list (append (delta-object-data obj) args)))
    (delta-object-data-set! obj new-list)))

;; List new: [...args...]
(define (m-list-new obj args ns interp)
  (new-list-object interp args))

;; Since we don't have special syntax for list literals (yet?), we use
;; "(List new: ...)" with its contents to represent it.
(define (m-list-repr obj args ns interp)
  (let ((items-repr
         (string-join
          (map (lambda (x)
                 (delta-object-data (get-delta-object-repr x ns interp)))
               (delta-object-data obj)))))
    (new-string-object interp (format "(List new: ~a)" items-repr))))

(define *list-methods*
  (list (list "append" m-list-append)
        (list "as-string" m-list-repr) ;; !
        (list "length" m-list-length)
        (list "new" m-list-new)
        (list "repr" m-list-repr)))

;;; --- List prototype ---

(define make-list-proto
  (make-proto-maker 'list :default '()))
