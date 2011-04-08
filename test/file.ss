
(module file mzscheme
  (require "../file.ss"
           (planet "test.ss" ("schematics" "schemeunit.plt" 2 6))
           (planet "test.ss" ("dherman" "test.plt" 1)))
  
  (provide file-test)

  (define-syntax in-tmp-dir
    (syntax-rules ()
      [(_ . body)
       (in-new-directory "Planet-scripting-tests" . body)]))
  
  (define file-test
    (test-suite "file.ss"
      (test-case "path-wrt-path"
        (check-equal? (path-wrt-path "/home/ryan" "subdir")
                       (build-path "/" "home" "ryan" "subdir"))
        (check-equal? (path-wrt-path "/home/ryan" "/tmp")
                       (build-path "/" "tmp")))
      (test-case "replace-file-extension"
        (check-equal? (replace-file-extension "foo.txt" "jpg")
                       (string->path "foo.jpg"))
        (check-equal? (replace-file-extension "/home/foo.txt" "jpg")
                       (string->path "/home/foo.jpg")))
      (test-case "pathlike-append"
        (check-equal? (pathlike-append "here/"
                                        "and"
                                        (string->path "there"))
                       (string->path "here/andthere")))
      ;; Tests with filesystem effects
      (test-case "touch"
        (in-tmp-dir
         (touch "foo.tmp")
         (check-true (file-exists? "foo.tmp"))
         (check-equal? (file-size "foo.tmp") 0)))
      (test-case "check-file-exists"
        (in-tmp-dir
         (touch "foo.tmp")
         (check-file-exists 'testing "foo.tmp")
         (check-exn exn:fail:filesystem?
                     (lambda () (check-file-exists 'testing "bar.tmp")))))
      (test-case "directory-list/sorted"
        (in-tmp-dir
         (touch "b")
         (touch "a")
         (check-equal? (map path->string (directory-list/sorted))
                        '("a" "b"))))
      (test-case "directory-list/paths and directory-list/absolute"
        (in-tmp-dir
         (make-directory "TestingDLP")
         (touch (build-path "TestingDLP" "a"))
         (touch (build-path "TestingDLP" "b"))
         (check-equal?
          (directory-list/paths "TestingDLP")
          (list (build-path "TestingDLP" "a")
                (build-path "TestingDLP" "b")))
         (check-equal? 
          (directory-list/absolute "TestingDLP")
          (list (build-path (current-directory) "TestingDLP" "a")
                (build-path (current-directory) "TestingDLP" "b")))))
      (test-case "newer?"
        (in-tmp-dir
         (with-output-to-file "old" (lambda () (newline)))
         (sleep 1)
         (with-output-to-file "new" (lambda () (newline)))
         (check-true (file-exists? "old"))
         (check-true (file-exists? "new"))
         (check-true (newer? "new" "old") "new is newer")
         (check-false (newer? "old" "new") "old is not newer")
         (check-true (newer? "new" "not-there") 
                      "new is newer than anything not there")
         (check-false (newer? "not-there" "old")
                       "not-there is not newer than something old")))
      ))
  )
