;; Copyright 2007, 2011 Ryan Culpepper
;; Released under the terms of the LGPL version 3 or later.

#lang setup/infotab
(define name "scripting")
(define compile-omit-paths '("test"))
(define blurb
  '("Scripting is a library of functions useful for writing "
    "racket scripts, especially those which execute "
    "external commands and manipulate the filesystem."))
(define scribblings '(("scribblings/scripting.scrbl")))
(define categories '(io misc))
(define can-be-loaded-with 'all)
(define required-core-version "5.1")
