#lang racket/base
(require racket/contract)

;; TODO: numerics: separate digits per locale

;; TODO: %float option to replace trailing 0s with spaces (for alignment)

(define (non-empty-string? x)
  (and (string? x) (positive? (string-length x))))

(define limit-mode/c
  (or/c 'truncate non-empty-string?))
(define justify-mode/c
  (or/c 'left 'right 'center))
(define pad-mode/c
  (or/c char? non-empty-string?))

(define sign-mode/c
  (or/c #f '+ '+0 'space))

;; Precision is one of
;;   Nat, for "up to N"
;;   '(exactly N), for "exactly N"
(define precision/c
  (or/c exact-nonnegative-integer?
        (list/c 'exactly exact-nonnegative-integer?)))

(define %string-contract
  (->* (string?)
       (#:limit (or/c exact-nonnegative-integer? +inf.0)
        #:limit-mode limit-mode/c
        #:width exact-nonnegative-integer?
        #:justify justify-mode/c
        #:left-pad pad-mode/c
        #:right-pad pad-mode/c)
       string?))
(define %integer-contract
  (->* (exact-integer?)
       (#:digits-width exact-positive-integer?
        #:digits-pad pad-mode/c
        #:sign sign-mode/c
        #:base (or/c 2 8 10 16)
        #:width exact-nonnegative-integer?
        #:justify justify-mode/c
        #:left-pad pad-mode/c
        #:right-pad pad-mode/c)
       string?))
(define %float-contract
  (->* (rational?)
       (#:precision precision/c
        #:digits-width exact-positive-integer?
        #:digits-pad pad-mode/c
        #:sign sign-mode/c
        #:width exact-nonnegative-integer?
        #:justify justify-mode/c
        #:left-pad pad-mode/c
        #:right-pad pad-mode/c)
       string?))

(provide/contract
 [%string %string-contract]
 [%integer %integer-contract]
 [%float %float-contract]

 [%limit
  (-> string?
      #:limit (or/c exact-nonnegative-integer? +inf.0)
      #:limit-mode limit-mode/c
      string?)]
 [%pad
  (->* (string?
        #:width exact-nonnegative-integer?
        #:justify justify-mode/c)
       (#:left-pad pad-mode/c
        #:right-pad pad-mode/c)
       string?)])

;; ----------------------------------------

(define (%limit #:limit [limit +inf.0]
                #:limit-mode [limit-mode 'truncate]
                s)
  (cond [(> (string-length s) limit)
         (cond [(eq? limit-mode 'truncate)
                (substring s 0 limit)]
               [(string? limit-mode) ;; contract has checked that |limit-mode| <= limit
                (string-append (substring s 0 (- limit (string-length limit-mode)))
                               limit-mode)]
               [else
                (error '%limit "unknown limit-mode: ~e" limit-mode)])]
        [else s]))

(define (%pad #:width minimum-width
              #:justify justify-mode
              #:left-pad [left-pad-mode #\space]
              #:right-pad [right-pad-mode #\space]
              s)
  (let* ([s-length (string-length s)]
         [to-pad-length (max 0 (- minimum-width s-length))])
    (let-values ([(left-pad-length right-pad-length)
                  (case justify-mode
                    ((left) (values 0 to-pad-length))
                    ((right) (values to-pad-length 0))
                    ((center)
                     (values (ceiling (/ to-pad-length 2))
                             (floor (/ to-pad-length 2)))))])
      (string-append
       (build-padding 'left left-pad-mode left-pad-length)
       s
       (build-padding 'right right-pad-mode right-pad-length)))))

(define (build-padding side pad-mode pad-length)
  (cond [(char? pad-mode)
         (make-string pad-length pad-mode)]
        [(and (string? pad-mode) (= (string-length pad-mode) 1))
         (build-padding side (string-ref pad-mode 0) pad-length)]
        [(string? pad-mode)
         (let* ([pattern pad-mode]
                [pattern-length (string-length pattern)]
                [whole-copies (quotient pad-length pattern-length)]
                [part-length (remainder pad-length pattern-length)]
                [pattern-copies (for/list ([i (in-range whole-copies)]) pattern)])
           (apply string-append
                  ;; For left, start at start of string
                  ;; For right, end at end of string.
                  (case side
                    ((left)
                     (append pattern-copies
                             (list (substring pattern 0 part-length))))
                    ((right)
                     (cons (substring pattern (- pattern-length part-length) pattern-length)
                           pattern-copies)))))]
        [else
         (error '%pad "unknown pad mode: ~e" pad-mode)]))

(define (build-sign-part N sign-mode)
  (cond [(or (negative? N) (equal? -0.0 N))
         "-"]
        [(zero? N)
         (case sign-mode
           ((+0) "+")
           ((space) " ")
           (else ""))]
        [(positive? N)
         (case sign-mode
           ((+ +0) "+")
           ((space) " ")
           (else ""))]))

;; ----------------------------------------

(define (%string #:limit [limit +inf.0]
                 #:limit-mode [limit-mode 'truncate]
                 #:width [minimum-width 0]
                 #:justify [justify-mode 'left]
                 #:left-pad [left-pad-mode #\space]
                 #:right-pad [right-pad-mode #\space]
                 s)
  (when (and (string? limit-mode) (> (string-length limit-mode) limit))
    (error '%string "replacement string longer than limit (~s): ~e"
           limit limit-mode))
  (let* ([s* (%limit s #:limit limit #:limit-mode limit-mode)])
    (cond [(< (string-length s*) minimum-width)
           (%pad s
                 #:width minimum-width
                 #:justify justify-mode
                 #:left-pad left-pad-mode
                 #:right-pad right-pad-mode)]
          [else s*])))

(define (%integer N
                  ;; Numeric formatting
                  #:digits-width [digits-width 1] ;; minimum *digits*
                  #:digits-pad [digits-pad-mode #\0]
                  #:sign [sign-mode #f]
                  #:base [base 10]
                  ;; General formatting
                  #:width [width 0] ;; minimum *total*
                  #:justify [justify-mode 'right]
                  #:left-pad [left-pad-mode #\space]
                  #:right-pad [right-pad-mode #\space])
  (let* ([N (inexact->exact N)]
         [sign-part (build-sign-part N sign-mode)]
         [digits-part (number->string (abs N) base)])
    (%pad (string-append sign-part
                         (%pad digits-part
                               #:width digits-width
                               #:justify 'right
                               #:left-pad digits-pad-mode
                               #:right-pad #\space))
          #:width width
          #:justify justify-mode
          #:left-pad left-pad-mode
          #:right-pad right-pad-mode)))

;; Note: contract for N is rational?, so no infinities or NaNs
(define (%float N
                ;; Numeric formatting
                #:precision [precision 2] ;; *digits after dot*
                #:digits-width [digits-width 1] ;; minimum *digits* (including dot)
                #:digits-pad [digits-pad-mode #\0]
                #:sign [sign-mode #f]
                ;; General formatting
                #:width [width 0] ;; minimum *total*
                #:justify [justify-mode 'right]
                #:left-pad [left-pad-mode #\space]
                #:right-pad [right-pad-mode #\space])
  (let* ([N-abs (abs N)]
         [sign-part (build-sign-part N sign-mode)]
         [precision*
          (cond [(pair? precision) (cadr precision)]
                [else precision])]
         [exactly? (pair? precision)]
         [digits-part
          (cond [exactly?
                 (real->decimal-string N-abs precision*)]
                [else
                 (let* ([N* (inexact->exact (round (* N-abs (expt 10 precision*))))]
                        [needed-precision
                         (let loop ([np precision*] [N* N*])
                           (cond [(zero? np) 0]
                                 [(zero? (remainder N* 10))
                                  (loop (sub1 np) (quotient N* 10))]
                                 [else np]))])
                   (cond [(positive? needed-precision)
                          (real->decimal-string N-abs needed-precision)]
                         [else ;; N-abs was an integer!
                          (number->string (inexact->exact (round N-abs)))]))])])
    (%pad (string-append sign-part
                         (%pad digits-part
                               #:width digits-width
                               #:justify 'right
                               #:left-pad digits-pad-mode
                               #:right-pad #\space))
          #:width width
          #:justify justify-mode
          #:left-pad left-pad-mode
          #:right-pad right-pad-mode)))

;; ----------------------------------------

(provide/contract
 [format-byte-count
  (-> exact-integer? string?)])

(define (format-byte-count n)
  (let ([absn (abs n)])
    (cond [(< absn #e1e4)
           (format "~s bytes" n)]
          [(< absn #e1e7)
           (format "~s KB" (quotient n #e1e3))]
          [(< absn #e1e10)
           (format "~s MB" (quotient n #e1e6))]
          [(< absn #e1e13)
           (format "~s GB" (quotient n #e1e9))]
          [else
           (format "~s TB" (quotient n #e1e12))])))
