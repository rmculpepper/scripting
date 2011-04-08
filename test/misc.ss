
(module misc mzscheme
  (require "../misc.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 6)))
  (provide misc-test)
  
  (define misc-test
    (test-suite "misc.ss"
      (test-case "map/separated"
        (let ((sep (lambda () 's)))
          (check-equal? (map/separated values sep '()) '())
          (check-equal? (map/separated values sep '(a)) '(a))
          (check-equal? (map/separated values sep '(a b c d e))
                        '(a s b s c s d s e))))
      (test-case "for-each/separated"
        (let ((mailbox null)
              (n 0))
          (let ((add (lambda (x) (set! mailbox (cons x mailbox))))
                (sep (lambda () 
                       (set! mailbox (cons n mailbox)) 
                       (set! n (add1 n)))))
            (for-each/separated add sep '(a b c d e))
            (check-equal? (reverse mailbox) '(a 0 b 1 c 2 d 3 e)))))
      (test-case "format-byte-count"
        (check-equal? (format-byte-count 12) "12 bytes")
        (check-equal? (format-byte-count 20000) "20 KB")
        (check-equal? (format-byte-count 20000000) "20 MB")
        (check-equal? (format-byte-count 20000000000) "20 GB"))
      (test-case "hash-table-put-fresh!"
        (let ((h (make-hash-table)))
          (hash-table-put! h 'a 1)
          (hash-table-put-fresh! h 'b 2)
          (check-exn exn:fail?
                     (lambda () (hash-table-put-fresh! h 'a 3)))))))
  )
