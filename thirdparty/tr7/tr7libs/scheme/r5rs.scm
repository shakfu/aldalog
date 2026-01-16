(define-library (scheme r5rs)

  (import (only (scheme base)
     < <= = > >= - / * + abs and append apply assoc assq assv begin boolean?
     caar cadr call-with-current-continuation call-with-values car case cdar
     cddr cdr ceiling char<=? char<? char=? char>=? char>? char? char->integer
     char-ready? close-input-port close-output-port complex? cond cons
     current-input-port current-output-port define define-syntax
     do dynamic-wind eof-object? eq? equal? eqv? even? exact exact? expt floor
     for-each gcd if inexact inexact? input-port? integer? integer->char lambda
     lcm length let let* letrec letrec-syntax let-syntax list list? list-ref
     list->string list-tail list->vector make-string make-vector map max member
     memq memv min modulo negative? newline not null? number? number->string
     odd? or output-port? pair? peek-char positive? procedure?
     quasiquote quote quotient rational? read-char real? remainder
     reverse round set! set-car! set-cdr! string string<=? string<? string=?
     string>=? string>? string? string-append string-copy string-fill!
     string-length string->list string->number string-ref string-set!
     string->symbol substring symbol? symbol->string truncate values vector
     vector? vector-fill! vector-length vector->list vector-ref vector-set!
     write-char zero?))
   (export
     < <= = > >= - / * + abs and append apply assoc assq assv begin boolean?
     caar cadr call-with-current-continuation call-with-values car case cdar
     cddr cdr ceiling char<=? char<? char=? char>=? char>? char? char->integer
     char-ready? close-input-port close-output-port complex? cond cons
     current-input-port current-output-port define define-syntax
     do dynamic-wind eof-object? eq? equal? eqv? even? (rename exact inexact->exact) exact? expt floor
     for-each gcd if (rename inexact exact->inexact) inexact? input-port? integer? integer->char lambda
     lcm length let let* letrec letrec-syntax let-syntax list list? list-ref
     list->string list-tail list->vector make-string make-vector map max member
     memq memv min modulo negative? newline not null? number? number->string
     odd? or output-port? pair? peek-char positive? procedure?
     quasiquote quote quotient rational? read-char real? remainder
     reverse round set! set-car! set-cdr! string string<=? string<? string=?
     string>=? string>? string? string-append string-copy string-fill!
     string-length string->list string->number string-ref string-set!
     string->symbol substring symbol? symbol->string truncate values vector
     vector? vector-fill! vector-length vector->list vector-ref vector-set!
     write-char zero?)

  (cond-expand
    (ratios
       (import (only (scheme base) denominator numerator rationalize))
        (export denominator numerator rationalize)))

  (cond-expand
    ((library (scheme char))
       (import (only (scheme char)
          char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
          char-downcase char-lower-case? char-numeric? char-upcase char-upper-case?
          char-whitespace? string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?))
        (export
          char-alphabetic? char-ci<=? char-ci<? char-ci=? char-ci>=? char-ci>?
          char-downcase char-lower-case? char-numeric? char-upcase char-upper-case?
          char-whitespace? string-ci<=? string-ci<? string-ci=? string-ci>=? string-ci>?)))

  (cond-expand
    ((library (scheme complex))
        (import (scheme complex))
         (export angle imag-part magnitude make-polar make-rectangular real-part)))

  (cond-expand
    ((library (scheme cxr))
       (import (scheme cxr))
        (export
          caaaar caaadr caaar caadar caaddr caadr cadaar cadadr cadar caddar cadddr caddr
          cdaaar cdaadr cdaar cdadar cdaddr cdadr cddaar cddadr cddar cdddar cddddr cdddr)))

  (cond-expand
    ((library (scheme eval))
       (import (only (scheme eval) eval))
        (export eval)))

  (cond-expand
    ((library (scheme file))
       (import (only (scheme file)
          call-with-input-file call-with-output-file open-input-file open-output-file
          with-input-from-file with-output-to-file))
        (export
          call-with-input-file call-with-output-file open-input-file open-output-file
          with-input-from-file with-output-to-file)))

  (cond-expand
    ((library (scheme inexact))
       (import (only (scheme inexact) acos asin atan cos exp log sin sqrt tan))
        (export acos asin atan cos exp log sin sqrt tan)))

  (cond-expand
    ((library (scheme lazy))
       (import (only (scheme lazy) delay force))
        (export delay force)))

  (cond-expand
    ((library (scheme load))
       (import (scheme load))
        (export load)))

  (cond-expand
    ((library (scheme read))
       (import (scheme read))
        (export read)))

  (cond-expand
    ((library (scheme repl))
       (import (scheme repl))
        (export interaction-environment)))

  (cond-expand
    ((library (scheme write))
       (import (only (scheme write) display write))
        (export display write)))

  (cond-expand
    ((library (scheme eval))
      (import (only (scheme eval) environment))
      (export null-environment scheme-report-environment)

      (begin

        (define (scheme-report-environment version)
          (if (equal? version 5)
            (environment '(scheme r5rs))
            (error "unsupported version" version)))

        (define (null-environment version)
          (if (equal? version 5)
            (environment '(only (scheme base)
                and begin case cond cond-expand define
                define-library define-record-type define-syntax
                define-values do guard if import include
                include-ci lambda let let* letrec letrec*
                letrec-syntax let-syntax let*-values let-values
                or quasiquote quote set!  syntax-error unless
                when))
            (error "unsupported version" version))))))
)

