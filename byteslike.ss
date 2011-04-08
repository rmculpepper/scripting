
(module byteslike mzscheme
  (provide (all-defined))

  ;; A byteslike value is one of
  ;;   - bytes
  ;;   - string

  ;; byteslike? : any -> boolean
  (define (byteslike? v)
    (or (bytes? v) (string? v)))
  
  ;; byteslike->bytes : (union bytes string) -> bytes
  (define (byteslike->bytes x)
    (cond [(bytes? x) x]
          [(string? x) (string->bytes/utf-8 x)]
          [else (raise-type-error 'byteslike->bytes "byteslike" x)]))
  
  ;; byteslike->string : (union bytes string) -> string
  (define (byteslike->string x)
    (cond [(string? x) x]
          [(bytes? x) (bytes->string/utf-8 x)]
          [else (raise-type-error 'byteslike->string "byteslike" x)]))
  
  ;; byteslike-append : byteslike ... -> byteslike
  (define (byteslike-append . items)
    (apply bytes-append (map byteslike->bytes items)))
  )
