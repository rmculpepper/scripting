(module error mzscheme
  (require "../error.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 6)))
  (provide error-test)
  (define error-test
    (test-suite "error.ss"
      (test-case 
       "raise-file-not-found"
       (check-exn exn:fail:filesystem?
                  (lambda () (raise-file-not-found 'testing "there"))))))
  )
