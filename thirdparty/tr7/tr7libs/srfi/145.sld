; copied from https://srfi.schemers.org/srfi-145/srfi-145.html
(define-library (srfi 145)
  (export assume)
  (import (scheme base))
  (begin
    (define-syntax assume
      (syntax-rules ()
        ((assume expression message ...)
         (or expression
             (fatal-error "invalid assumption" (quote expression) (list message ...))))
        ((assume . _)
         (syntax-error "invalid assume syntax"))))
  (cond-expand
    (debug
     (begin
       (define fatal-error error)))
    (else
     (begin
       (define (fatal-error message . objs)
         (car 0)))))))

