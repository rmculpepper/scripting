
(module read mzscheme
  (require "../read.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 6)))
  (provide read-test)
  
  (define text "1 (a b)")
  
  (define read-test
    (test-suite "read.ss"
      (test-case "read-all"
        (check-equal? (read-all (open-input-string ""))
                      '())
        (check-equal? (read-all (open-input-string text))
                      '(1 (a b))))
      ;; read-all/file not tested
      (test-case "read-all-syntax"
        (let ((stxs (read-all-syntax #f (open-input-string text))))
          (check-true (list? stxs))
          (check-true (andmap syntax? stxs))
          (check-equal? (map syntax-object->datum stxs)
                        '(1 (a b)))))
      ;; read-all-syntax/file not tested
      ))
  )
