
(module read mzscheme
  (provide (all-defined))

  (define read-all
    (case-lambda
      [() (read-all (current-input-port))]
      [(iport)
       (let loop ()
         (let ([next (read iport)])
           (if (eof-object? next)
               null
               (cons next (loop)))))]))

  (define (read-all/file file)
    (with-input-from-file file
      read-all))
  
  (define read-all-syntax
    (case-lambda
      [() (read-all-syntax #f)]
      [(source) (read-all-syntax source (current-input-port))]
      [(source iport)
       (let loop ()
         (let ([next (read-syntax source iport)])
           (if (eof-object? next)
               null
               (cons next (loop)))))]))
  
  (define (read-all-syntax/file file)
    (call-with-input-file file
      (lambda (iport) (read-all-syntax file iport))))
  )
