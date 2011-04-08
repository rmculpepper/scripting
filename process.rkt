#lang racket/base
(require racket/contract
         racket/system)
(provide/contract
 [system/exit-code+output
  (-> string? (values exact-nonnegative-integer? string?))]
 [system/output
  (-> string? string?)])

(define (system/exit-code+output command)
  (let* ([out (open-output-string)]
         [control (process/ports out #f #f command)]
         [p (list-ref control 4)])
    (p 'wait)
    (values (p 'exit-code)
            (get-output-string out))))

(define (system/output command)
  (let-values ([(e out) (system/exit-code+output command)])
    out))
