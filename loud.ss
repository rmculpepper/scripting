;; loud
;; Filesystem, process, etc functions that print out their 
;; arguments.

(module loud mzscheme
  (require (lib "etc.ss")
           (lib "file.ss")
           (lib "process.ss")
           "pathlike.ss"
           "misc.ss")
  (provide (all-defined))
  
  (define loud? (make-parameter #t))
  
  (define (loud:wrap-strings-procedure proc)
    (lambda args
      (when (loud?)
        (for-each/separated display (lambda () (display " ")) args)
        (newline))
      (apply proc args)))
  
  (define (loud:wrap-pathlikes-procedure/message proc msg)
    (lambda args
      (when (loud?)
        (display msg)
        (display ": ")
        (for-each/separated (compose display pathlike->string)
                            (lambda () (display " ")) 
                            args)
        (newline))
      (apply proc args)))
  
  (define loud:make-directory
    (loud:wrap-pathlikes-procedure/message
     make-directory
     "make-directory"))
  (define loud:delete-directory
    (loud:wrap-pathlikes-procedure/message
     delete-directory
     "delete-directory"))
  (define loud:copy-file
    (loud:wrap-pathlikes-procedure/message 
     copy-file
     "copy-file"))
  (define loud:make-file-or-directory-link
    (loud:wrap-pathlikes-procedure/message
     make-file-or-directory-link
     "make-file-or-directory-link"))
  (define loud:delete-file
    (loud:wrap-pathlikes-procedure/message
     delete-file
     "delete-file"))
  (define loud:rename-file-or-directory
    (loud:wrap-pathlikes-procedure/message
     rename-file-or-directory
     "rename-file-or-directory"))

  (define loud:process
    (loud:wrap-strings-procedure process))
  (define loud:process*
    (loud:wrap-strings-procedure process*))
  (define loud:system
    (loud:wrap-strings-procedure system))
  (define loud:system*
    (loud:wrap-strings-procedure system*))
  (define loud:system/exit-code
    (loud:wrap-strings-procedure system/exit-code))
  (define loud:system*/exit-code
    (loud:wrap-strings-procedure system*/exit-code))
  
  (define loud:make-directory*
    (loud:wrap-pathlikes-procedure/message 
     make-directory*
     "make-directory*"))
  (define loud:copy-directory/files
    (loud:wrap-pathlikes-procedure/message 
     copy-directory/files
     "copy-directory/files*"))
  (define loud:delete-directory/files
    (loud:wrap-pathlikes-procedure/message 
     delete-directory/files 
     "delete-directory/files"))
  )
