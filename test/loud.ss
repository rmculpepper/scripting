
(module loud mzscheme
  (require "../loud.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 6)))
  (provide loud-test)
  
  (define loud-test
    (test-suite "loud.ss"
      (test-case "string procedures"
        (let ((out (open-output-string))
              (loudmouth (loud:wrap-strings-procedure string-append)))
          (parameterize ((current-output-port out))
            (check-equal? (parameterize ((loud? #f)) (loudmouth "a" "b"))
                          "ab")
            (check-equal? (get-output-string out) "")
            (check-equal? (loudmouth "a" "b") "ab")
            (check-equal? (get-output-string out) "a b\n"))))
      (test-case "pathlike procedures"
        (let ((out (open-output-string))
              (loudmouth (loud:wrap-pathlikes-procedure/message cons "talk")))
          (parameterize ((current-output-port out))
            (check-equal? (parameterize ((loud? #f)) (loudmouth "a" "b"))
                          (cons "a" "b"))
            (check-equal? (get-output-string out) "")
            (check-equal? (loudmouth "a" "b") (cons "a" "b"))
            (check-equal? (get-output-string out) "talk: a b\n"))))
      ))
  )
