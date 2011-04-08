
(module pathlike mzscheme
  (provide (all-defined))
  
  ;; A pathlike value is one of 
  ;;   - path
  ;;   - string
  
  ;; pathlike? : any -> boolean
  (define (pathlike? v)
    (or (path? v) (string? v)))
  
  ;; pathlike->bytes : pathlike -> bytes
  (define (pathlike->bytes x)
    (cond [(path? x) (path->bytes x)]
          [(string? x) (path->bytes (string->path x))]
          [else (raise-type-error 'pathlike->bytes "pathlike" x)]))
  
  ;; pathlike->string : pathlike -> string
  (define (pathlike->string x)
    (cond [(path? x) (path->string x)]
          [(string? x) x]
          [else (raise-type-error 'pathlike->string "pathlike" x)]))
  
  ;; pathlike->path : pathlike -> path
  (define (pathlike->path x)
    (cond [(path? x) x]
          [(string? x) (string->path x)]
          [else (raise-type-error 'pathlike->path "pathlike" x)]))
  
  )
