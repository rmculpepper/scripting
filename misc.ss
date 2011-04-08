
(module misc mzscheme
  (require (lib "process.ss"))
  (provide (all-defined))
  
  (define (map/separated proc separator items)
    (if (null? items)
        null
        (cons
         (proc (car items))
         (let loop ([items (cdr items)])
           (if (null? items)
               null
               (cons (separator)
                     (cons (proc (car items))
                           (loop (cdr items)))))))))
  
  (define (for-each/separated proc separator items)
    (if (null? items)
        (void)
        (begin
          (proc (car items))
          (let loop ([items (cdr items)])
            (if (null? items)
                (void)
                (begin
                  (separator)
                  (proc (car items))
                  (loop (cdr items))))))))
  
  (define (format-byte-count n)
    (cond [(< -1 n 10000)
           (format "~s bytes" n)]
          [(< n 1000000)
           (format "~s KB" (quotient n 1000))]
          [(< n 1000000000)
           (format "~s MB" (quotient n 1000000))]
          [else
           (format "~s GB" (quotient n 1000000000))]))
  
  (define (hash-table-put-fresh! ht key val)
    (let/ec k
      (hash-table-get ht key
                      (lambda _ 
                        (hash-table-put! ht key val)
                        (k (void))))
      (error 'hash-table-put-fresh! "key already exists")))
  
  (define (system/exit-code+output command)
    (let* ([out (open-output-string)]
           [control (process/ports out #f #f command)]
           [p (list-ref control 4)])
      (p 'wait)
      (values (p 'exit-code)
              (get-output-string out))))

  (define (system/output command)
    (let-values ([(e out) (system/exit-code+output command)])
      out))
                      
  )
