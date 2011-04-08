
(module file mzscheme
  (require "byteslike.ss"
           "pathlike.ss"
           "error.ss"
           (lib "list.ss")
           (lib "file.ss"))
  (provide (all-defined))

  ;; path-wrt-path : pathlike pathlike -> path
  ;; Interprets the second path with respect to the first.
  ;; If the second path is absolute, the first is ignored.
  (define (path-wrt-path base path)
    (if (absolute-path? path)
        (pathlike->path path)
        (build-path base path)))
  
  ;; replace-file-extension : pathlike byteslike -> path
  (define (replace-file-extension file new-ext)
    (path-replace-suffix (pathlike->path file)
                         (byteslike-append "." new-ext)))
  
  ;; pathlike-append : pathlike ... -> path
  (define (pathlike-append . pathlikes)
    (bytes->path (apply bytes-append (map pathlike->bytes pathlikes))))

  ;; touch : pathlike -> void
  (define (touch file)
    (unless (file-exists? file)
      (close-output-port (open-output-file file 'error))))
  
  ;; check-file-exists : symbol pathlike -> void
  (define (check-file-exists name path)
    (unless (file-exists? path)
      (raise-file-not-found name path)))
  
  ;; directory-list/sorted : [pathlike] -> (list-of path)
  (define (directory-list/sorted . args)
    (map bytes->path 
         (quicksort (map pathlike->bytes (apply directory-list args))
                    bytes<?)))
  
  ;; directory-list/paths : pathlike -> (list-of path)
  (define (directory-list/paths directory)
    (map (lambda (entry) (path-wrt-path directory entry))
         (directory-list/sorted directory)))
  
  ;; directory-list/absolute : pathlike -> (list-of absolute-path)
  (define (directory-list/absolute directory)
    (let ((base (path-wrt-path (current-directory) directory)))
      (map (lambda (entry) (build-absolute-path base entry))
           (directory-list/sorted directory))))
    
  ;; newer? : pathlike pathlike -> boolean
  (define (newer? a b)
    (and (file-exists? a)
         (or (not (file-exists? b))
             (>= (file-or-directory-modify-seconds a)
                 (file-or-directory-modify-seconds b)))))
  
  )
