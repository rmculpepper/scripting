#lang scribble/doc
@(require scribble/manual
          scribble/struct
          scribble/eval
          planet/scribble
          planet/version
          (for-label racket/base
                     racket/contract
                     racket/system
                     racket/file
                     (this-package-in file)
                     (this-package-in loud)
                     (this-package-in process)
                     (this-package-in format)))

@(begin
  (define the-eval (make-base-eval))
  (the-eval '(require racket/math "format.rkt" "file.rkt" "format.rkt")))

@(define the-version
   (format "~a.~a" (this-package-version-maj) (this-package-version-min)))

@title[#:version the-version]{scripting: Scripting utilities}
@author[@author+email["Ryan Culpepper" "ryanc@racket-lang.org"]]

@section-index["script" "scripting"]

@defmodule/this-package[main]

This package provides utilities for writing scripts, including
functions for file operations, process invocation, and producing
formatted output.


@; {------------------------------------------------------------}

@section{Files and paths}

@defmodule/this-package[file]

@deftogether[[
@defproc[(path-string->bytes [path path-string?]) bytes?]
@defproc[(path-string->string [path path-string?]) string?]
@defproc[(path-string->path [path path-string?]) path?]
]]{

  Converts @racket[path] to a bytes, string, or path, respectively.
}

@defproc[(raise-file-not-found [who symbol?] [file path-string?])
         any]{

  Raises an error with the message @racket{file not found} and the
  given @racket[file] name.
}

@defproc[(path-wrt-path [base path-string?]
                        [path path-string?])
         path?]{

  Interprets @racket[path] with respect to @racket[base]. If
  @racket[path] is absolute, then @racket[base] is ignored. The
  resulting path is relative if both input paths were relative;
  otherwise, it is absolute.

@examples[#:eval the-eval
(path-wrt-path "here" "there")
(path-wrt-path "here" "/tmp/there")
(path-wrt-path "/home/me" "there")
(path-wrt-path "/home/me" "/tmp/there")
]
}

@defproc[(replace-file-extension [path path-string?]
                                 [ext string?])
         path?]{

  Like @racket[path-replace-suffix], but only replaces the part of the
  name after the dot.

@examples[#:eval the-eval
(replace-file-extension "a.b" "c")
]
}

@defproc[(path-string-append [path path-string?] ...)
         path?]{

  Like @racket[string-append], but accepts a mixture of paths and
  strings, and concatenates them to produce a path. (Unlike
  @racket[build-path], no separators are inserted between the
  components.)

@examples[#:eval the-eval
(path-string-append "email" "-number-" (string->path "45.txt"))
]
}

@defproc[(touch [path path-string?])
         void?]{

  Creates a file named @racket[path] or updates the modification time
  of the existing file.
}

@defproc[(check-file-exists [who symbol?]
                            [path path-string?])
         void?]{

  If @racket[path] does not exist, raises an exception using
  @racket[raise-file-not-found].
}

@defproc[(directory-list/sorted [dir path-string? (current-directory)])
         (listof path?)]{

  Returns the sorted contents of the directory @racket[dir].
}

@defproc[(directory-list/paths [dir path-string? (current-directory)])
         (listof path?)]{

  Returns the contents of directory @racket[dir] as paths, such that
  the paths, interpreted with respect to the current directory, point
  to the contents of the given directory. That is, if @racket[dir] is
  a relative path, the resulting paths are relative; if @racket[dir]
  is an absolute path, the resulting paths are absolute.
}

@defproc[(directory-list/absolute [dir path-string?])
         (listof absolute-path?)]{

  Returns a list of absolute paths referring to the contents of the
  directory @racket[dir], which may be given as either relative or
  absolute.
}

@defproc[(newer? [file1 path-string?] [file2 path-string?])
         boolean?]{

Returns true if either
@itemlist[
@item{@racket[file1] exists but @racket[file2] does not; or}
@item{both @racket[file1] and @racket[file2] exist, and
  @racket[file1]'s modification time is later than @racket[file2]'s
  modification time}
]
}


@; {------------------------------------------------------------}

@section{Loud operations}

@defmodule/this-package[loud]

It is often helpful for a script to echo to the screen the actions it
is performing on the filesystem and the command lines it is
invoking. This module defines versions of many filesystem and process
procedures that echo their arguments to the current output port.

For example, executing @racket[(loud:delete-file "foo.txt")] will
print out @tt{delete-file: "foo.txt"} when the procedure is called.

@(require (for-syntax racket/base))
@;(define-syntax-rule (defit* id) (defidentifier #'id))
@(define-syntax-rule (defit* id) (defthing id procedure?))

@deftogether[[
@defit*[loud:make-directory]
@defit*[loud:delete-directory]
@defit*[loud:copy-file]
@defit*[loud:make-file-or-directory-link]
@defit*[loud:delete-file]
@defit*[loud:rename-file-or-directory]
@defit*[loud:make-directory*]
@defit*[loud:copy-directory/files]
@defit*[loud:delete-directory/files]]]{

  Loud variants of @racket[make-directory], @racket[delete-directory],
  @racket[copy-file], @racket[make-file-or-directory-link],
  @racket[delete-file], @racket[rename-file-or-directory],
  @racket[make-directory*], @racket[copy-directory/files], and
  @racket[delete-directory/files], respectively.
}

@deftogether[[
@defit*[loud:system]
@defit*[loud:system*]
@defit*[loud:system/exit-code]
@defit*[loud:process]
@defit*[loud:process*]
@defit*[loud:system/exit-code+output]
@defit*[loud:system/output]]]{

  Loud variants of @racket[system], @racket[system*],
  @racket[system/exit-code], @racket[process], @racket[process*],
  @racket[system/exit-code+output], and @racket[system/output],
  respectively.
}


@; {------------------------------------------------------------}

@section{Process invocation}

@defmodule/this-package[process]

@defproc[(system/exit-code+output [command string?])
         (values (integer-in 0 255) string?)]{

  Like @racket[system/exit-code], but also returns the output produced
  by the process as a string.
}

@defproc[(system/output [command string?])
         string?]{

  Like @racket[system], but returns the output produced by the process
  as a string.
}


@; {------------------------------------------------------------}

@section{Formatting}

@defmodule/this-package[format]

@defproc[(%limit [s string?]
                 [#:limit limit (or/c exact-nonnegative-integer? +inf.0)]
                 [#:limit-mode limit-mode (or/c 'truncate string?)])
         string?]{

Produces a string of at most @racket[limit] characters. If the length
of @racket[s] is less than or equal to @racket[limit], then @racket[s]
is returned. If the length is greater than @racket[limit], then
@racket[s] is processed according to @racket[limit-mode] to produce a
string of exactly @racket[limit] characters.

@itemize[

@item{If @racket[limit-mode] is @racket['truncate], any characters
over @racket[limit] are dropped, and the @racket[limit]-length prefix
of @racket[s] is returned.}

@item{If @racket[limit-mode] is a string, then it is appended to the
end of a prefix of @racket[s] such that the resulting string is
exactly @racket[limit] characters long. If the length of
@racket[limit-mode] is greater than @racket[limit], an error is
raised.}
]

@examples[#:eval the-eval
(%limit #:limit 15 #:limit-mode 'truncate "The quick brown fox")
(%limit #:limit 15 #:limit-mode "..." "The quick brown fox")
]

}

@defproc[(%pad [s string?]
               [#:width width exact-nonnegative-integer?]
               [#:justify justify (or/c 'left 'right 'center)]
               [#:left-pad left-pad (or/c char? non-empty-string?) #\space]
               [#:right-pad right-pad (or/c char? non-empty-string?) #\space])
         string?]{

Produces a string of at least @racket[width] characters. If the length
of @racket[s] is greater than or equal to @racket[width], then
@racket[s] is returned. Otherwise, @racket[s] is padded to exactly
@racket[width] characters.

If @racket[justify] is @racket['left], then only right-padding is
added; if @racket[justify] is @racket['right], then only left-padding
is added; and if @racket[justify] is @racket['center], then roughly
equal amounts of left-padding and right-padding are added.

Padding is specified as a non-empty string or as a single character,
which is treated as the corresponding string of length 1. Left-padding
consists of @racket[left-pad] repeated in its entirety as many times
as possible, and then the remaining space is filled by a @emph{prefix}
of @racket[left-pad]. In contrast, right-padding consists of a
@emph{suffix} of @racket[right-pad] followed by a number of copies of
@racket[right-pad] in its entirety. In short, left-padding starts with
the start of @racket[left-pad]; right-padding ends with the end of
@racket[right-pad].

@examples[#:eval the-eval
(%pad #:width 20 #:justify 'left "apple")
(%pad #:width 20 #:justify 'left #:right-pad " ." "pear")
(%pad #:width 20 #:justify 'right #:left-pad ". " "plum")
(%pad #:width 20 #:justify 'center #:left-pad "- " #:right-pad " -"
      "orange")
]

}


@;{----------------------------------------}

@defproc[(%string [s string?]
                  [#:limit limit (or/c exact-nonnegative-integer? +inf.0) +inf.0]
                  [#:limit-mode limit-mode (or/c 'truncate string?) 'truncate]
                  [#:width width exact-nonnegative-integer? 0]
                  [#:justify justify (or/c 'left 'right 'center) 'left]
                  [#:left-pad left-pad (or/c char? non-empty-string?) #\space]
                  [#:right-pad right-pad (or/c char? non-empty-string?) #\space])
         string?]{

Formats a string. The input string @racket[s] is first limited to at
most @racket[limit] characters, then padded (and justified) to at
least @racket[width]. Note that @racket[limit] may be less than
@racket[width].

Equivalent to:
@racketblock[
(%pad #:width width
      #:justify justify
      #:left-pad left-pad
      #:right-pad right-pad
      (%limit #:limit limit
              #:limit-mode limit-mode
              s))
]

}

@defproc[(%integer [x exact-integer?]
                   [#:digits-width digits-width exact-positive-integer? 1]
                   [#:digits-pad digits-pad (or/c char? non-empty-string?) #\0]
                   [#:sign sign (or/c #f '+ '++ 'space) #f]
                   [#:base base (or/c 2 8 10 16) 10]
                   [#:width width exact-nonnegative-integer? 0]
                   [#:justify justify (or/c 'left 'right 'center) 'right]
                   [#:left-pad left-pad (or/c char? non-empty-string?) #\space]
                   [#:right-pad right-pad (or/c char? non-empty-string?) #\space])
         string?]{

Formats an integer. Formatting is separated into two stages:
@emph{digit formatting} and @emph{field padding}.

@emph{Digit formatting} is concerned with the conventions of
formatting numbers. It is controlled by @racket[digits-width],
@racket[digits-pad], @racket[sign], and @racket[base].

@itemize[

@item{@racket[digits-width] is the minimum number of digits output,
not including the sign character. If @racket[x] would normally be
printed with fewer than @racket[digits-width] digits, the output is
padded according to @racket[digits-pad].}

@item{@racket[digits-pad] specifies the (left-aligned) padding
used to pad the formatting number to at least @racket[digits-width]
characters (not including the sign character). The padding is placed
between the sign and the normal digits of @racket[x].}

@item{@racket[sign] controls how the sign of the number is
indicated. In all cases, if the number is negative, a minus sign is
included. In the default mode, @racket[#f], no sign output is
generated for either positive numbers or zero. In mode @racket['+],
positive numbers are indicated with a plus sign, but no indicator is
generated for zero. In mode @racket['++], both positive numbers and
zero are indicated with a plus sign. Finally, in mode @racket['space],
both positive numbers and zero are prefixed with a space character.}

@item{@racket[base] controls the base that @racket[x] is formatted
in. If @racket[base] is @racket[16], then lower-case
@litchar{a}-@litchar{f} are used.}

]

@emph{Field padding} is concerned with the placement of text in a
field. It is controlled by @racket[width], @racket[justify],
@racket[left-pad], and @racket[right-pad]; the options have the same
meaning as in the @racket[%pad] function.

@examples[#:eval the-eval
(%integer 17)
(%integer 17 #:digits-width 10)
(%integer -22 #:digits-width 10 #:digits-pad #\space)
(%integer -22 #:width 11)
]
}


@defproc[(%float [x rational?]
                 ;; Numeric formatting
                 [#:precision precision 
                              (or/c exact-nonnegative-integer?
                                    (list/c 'exactly exact-nonnegative-integer?))
                              2]
                 [#:digits-width digits-width exact-positive-integer? 1]
                 [#:digits-pad digits-pad (or/c char? non-empty-string?) #\0]
                 [#:sign sign (or/c #f '+ '++ 'space) #f]

                 [#:width width exact-nonnegative-integer? 0]
                 [#:justify justify (or/c 'left 'right 'center) 'right]
                 [#:left-pad left-pad (or/c char? non-empty-string?) #\space]
                 [#:right-pad right-pad (or/c char? non-empty-string?) #\space])
         string?]{

Formats a rational number in fixed-point notation. Formatting is
separated into two stages: @emph{digit formatting} and @emph{field
padding}.

@emph{Digit formatting} is concerned with the conventions of
formatting numbers. It is controlled by @racket[precision],
@racket[digits-width], @racket[digits-pad], and @racket[sign].

@itemize[

@item{@racket[precision] controls the number of digits after the
decimal point. If @racket[precision] is given as a natural number,
then up to @racket[precision] digits are displayed, but trailing
zeroes are dropped, and if all digits after the decimal point are
dropped the decimal point is also dropped. If @racket[precision] is
given as @racket['(exactly _digits)], then exactly @racket[_digits]
digits after the decimal point are used.}

@item{@racket[digits-width], @racket[digits-pad], and @racket[sign]
are interpreted as in @racket[%integer], except that
@racket[digits-width] includes the decimal point.}

]

@emph{Field padding} is concerned with the placement of text in a
field. It is controlled by @racket[width], @racket[justify],
@racket[left-pad], and @racket[right-pad]; the options have the same
meaning as in the @racket[%pad] function.

@examples[#:eval the-eval
(%float pi)
(%float pi #:precision 4)
(%float 1.5 #:precision 4)
(%float 1.5 #:precision '(exactly 4))
(%float 1.5 #:precision '(exactly 4) #:digits-width 10)
]
}

@defproc[(format-byte-count [count exact-integer?])
         string?]{

  Abbreviates and formats @racket[count] as a byte count, using the
  tags @racket{bytes}, @racket{KB}, @racket{MB}, @racket{GB}, and
  @racket{TB}.

@examples[#:eval the-eval
(format-byte-count 1234)
(format-byte-count 12345)
(format-byte-count 123456)
(format-byte-count 1234567890)
]
}

@(close-eval the-eval)
