#lang racket/base
(require (for-syntax racket/base)
         racket/file
         racket/string
         racket/system
         "process.rkt")
(define loud? (make-parameter #t))

(define-syntax (define/provide-loud stx)
  (syntax-case stx ()
    [(_ fun)
     (with-syntax ([loud-name
                    (datum->syntax #'fun
                                   (string->symbol
                                    (format "loud:~a" (syntax-e #'fun))))])
       #'(begin
           (define loud-name
             (procedure-reduce-arity
              (let ([loud-name
                     (lambda args
                       (when (loud?)
                         (printf "~a: ~a\n" 'fun (string-join args " ")))
                       (apply fun args))])
                loud-name)
              (procedure-arity fun)))
           (provide loud-name)))]
    [(_ spec ...)
     #'(begin (define/provide-loud spec) ...)]))

(define/provide-loud
  make-directory
  delete-directory
  copy-file
  make-file-or-directory-link
  delete-file
  rename-file-or-directory

  system
  system*
  system/exit-code
  process
  process*

  system/exit-code+output
  system/output

  make-directory*
  copy-directory/files
  delete-directory/files)

(provide loud?)
