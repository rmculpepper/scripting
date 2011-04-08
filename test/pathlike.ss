
(module pathlike mzscheme
  (require "../pathlike.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 6)))
  (provide pathlike-test)
  
  (define pathlike-test
    (test-suite "pathlike.ss"
      (test-case "pathlike?"
        (check-true (pathlike? "a"))
        (check-true (pathlike? (string->path "a")))
        (check-false (pathlike? #"a"))
        (check-false (pathlike? 5))
        (check-false (pathlike? 'hello)))
      (test-case "pathlike->string"
        (check-equal? (pathlike->string (string->path "a")) "a")
        (check-equal? (pathlike->string "a") "a")
        (check-exn exn:fail:contract? 
                   (lambda () (pathlike->string #"a"))))
      (test-case "pathlike->path"
        (check-equal? (pathlike->path (string->path "a")) (string->path "a"))
        (check-equal? (pathlike->path "a") (string->path "a"))
        (check-exn exn:fail:contract? 
                   (lambda () (pathlike->path #"a"))))
      (test-case "pathlike->bytes"
        (check-equal? (pathlike->bytes (string->path "a")) #"a")
        (check-equal? (pathlike->bytes "a") #"a")
        (check-exn exn:fail:contract? 
                   (lambda () (pathlike->bytes #"a"))))))
  )
