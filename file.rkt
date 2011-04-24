#lang racket/base
(require racket/contract
         racket/file
         racket/list)

;; Recall: path-string = path | string (with constraints)
;; see path-string?

(provide/contract
 [path-string->bytes
  (-> path-string? bytes?)]
 [path-string->string
  (-> path-string? string?)]
 [path-string->path
  (-> path-string? path?)])

(define (path-string->bytes x)
  (cond [(path? x) (path->bytes x)]
        [(string? x) (path->bytes (string->path x))]))

(define (path-string->string x)
  (cond [(path? x) (path->string x)]
        [(string? x) x]))

(define (path-string->path x)
  (cond [(path? x) x]
        [(string? x) (string->path x)]))

;; ----------------------------------------

(provide/contract
 [raise-file-not-found
  (-> symbol? path-string?
      any)])

(define (raise-file-not-found who path)
  (let ([path
         (cond [(path? path) (path->string path)]
               [(string? path) path])])
    (error who "file not found: ~e" path)))
;; ----------------------------------------

(provide/contract
 [path-wrt-path
  (-> path-string? path-string?
      path-string?)]
 [replace-file-extension
  (-> path-string? string?
      path?)]
 [path-string-append
  (->* () () #:rest (listof path-string?)
       path?)]
 [touch
  (-> path-string? void?)]
 [check-file-exists
  (-> symbol? path-string?
      any)]
 [directory-list/sorted
  (->* () (path-string? #:compare procedure?)
       (listof path?))]
 [directory-list/paths
  (-> path-string?
      (listof path?))]
 [directory-list/absolute
  (-> path-string?
      (listof absolute-path?))]
 [newer?
  (-> path-string? path-string?
      boolean?)])

;; path-wrt-path : path-string path-string -> path
;; Interprets the second path with respect to the first.
;; If the second path is absolute, the first is ignored.
;; Like path->complete-path, except base need not be absolute.
(define (path-wrt-path base path)
  (if (absolute-path? path)
      (path-string->path path)
      (build-path base path)))

;; replace-file-extension : path-string string -> path
(define (replace-file-extension path new-ext)
  (path-replace-suffix path (string-append "." new-ext)))

;; path-string-append : path-string ... -> path
(define (path-string-append . path-strings)
  (bytes->path (apply bytes-append (map path-string->bytes path-strings))))

;; touch : path-string -> void
(define (touch file)
  (close-output-port (open-output-file file 'append)))

;; check-file-exists : symbol path-string -> void
(define (check-file-exists name path)
  (unless (file-exists? path)
    (raise-file-not-found name path)))

;; directory-list/sorted : [path-string] -> (list-of path)
(define (directory-list/sorted [base (current-directory)]
                               #:compare [<? string<?])
  (sort (directory-list base)
        <?
        #:key path->string
        #:cache-keys? #t))

;; directory-list/paths : path-string -> (list-of path)
(define (directory-list/paths directory)
  (map (lambda (entry) (path-wrt-path directory entry))
       (directory-list/sorted directory)))

;; directory-list/absolute : path-string -> (list-of absolute-path)
(define (directory-list/absolute directory)
  (let ((base (path-wrt-path (current-directory) directory)))
    (map (lambda (entry) (build-path base entry))
         (directory-list/sorted directory))))

;; newer? : path-string path-string -> boolean
(define (newer? a b)
  (and (file-exists? a)
       (or (not (file-exists? b))
           (>= (file-or-directory-modify-seconds a)
               (file-or-directory-modify-seconds b)))))
