
(module byteslike mzscheme
  (require "../byteslike.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 6)))
  
  (provide byteslike-test)
  (define byteslike-test
    (test-suite "byteslike.ss"
      (test-case "byteslike?"
        (check-true (byteslike? ""))
        (check-true (byteslike? #"a"))
        (check-false (byteslike? 5))
        (check-false (byteslike? 'hello))
        (check-false (byteslike? (bytes->path #"a"))))
      (test-case "byteslike->string"
        (check-equal? (byteslike->string "hello") "hello")
        (check-equal? (byteslike->string #"hello") "hello")
        (check-exn exn:fail:contract?
                   (lambda () (byteslike->string 'a))))
      (test-case "byteslike->bytes"
        (check-equal? (byteslike->bytes "hello") #"hello")
        (check-equal? (byteslike->bytes #"hello") #"hello")
        (check-exn exn:fail:contract?
                   (lambda () (byteslike->bytes 'hello))))
      (test-case "byteslike-append"
        (check-equal? (byteslike-append "a" #"b" #"c")
                      #"abc")
        (check-equal? (byteslike-append) #""))))
  )
