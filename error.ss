
(module error mzscheme
  (provide (all-defined))
  
  (define-struct (exn:fail:filesystem:path exn:fail:filesystem) (path))
  
  (define (raise-file-not-found name path)
    (raise (make-exn:fail:filesystem:path
            (string->immutable-string
             (format "~a: file not found: ~e" name path))
            (current-continuation-marks)
            path)))
  )
