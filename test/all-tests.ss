
(module all-tests mzscheme
  (require "byteslike.ss"
           "error.ss"
           "file.ss"
           "loud.ss"
           "misc.ss"
           "pathlike.ss"
           "read.ss")
  (require (planet "test.ss" ("schematics" "schemeunit.plt" 2 6))
           (planet "graphical-ui.ss" ("schematics" "schemeunit.plt" 2 6)))

  (provide all-tests)
  (define all-tests
    (test-suite "scripting.plt"
      byteslike-test
      error-test
      file-test
      loud-test
      misc-test
      pathlike-test
      read-test)))
