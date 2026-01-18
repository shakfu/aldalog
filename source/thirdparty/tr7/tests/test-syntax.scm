; defines C like for
(define-syntax for
   (syntax-rules (.. in)
      ((for var in low .. sup code ...) (for var low (< var sup) (+ var 1) code ...))
      ((for var in coll code ...)
                 (cond
                    ((list? coll)
                        (for-each (lambda (var) code ...) coll))
                    ((vector? coll)
                        (vector-for-each (lambda (var) code ...) coll))
                    ((string? coll)
                        (string-for-each (lambda (var) code ...) coll))
                    (else (error "unsupported for" coll))))
      ((for var low test inc code ...)  (do ((var low inc)) ((not test)) code ...))))

(for i in 0 .. 10
	(display i)
	(newline))

(for i in "abcd"
	(display i)
	(newline))

(for i in '(a z e r)
	(display i)
	(newline))

(for i in #(34 56 78)
	(display i)
	(newline))

(for i in 7
	(display i)
	(newline))

; -----------------------------------------------------------------------------
; from @retropikzel (#28)

(define-library (ex) (export check) (import (scheme base))
  (begin

    (define-syntax define-syntax-rule
      (syntax-rules ()
        ((define-syntax-rule (keyword args ...) body)
         (define-syntax keyword
           (syntax-rules ()
             ((keyword args ...) body))))))

    (define-syntax-rule (check expected actual)
       (lambda ()
         (let ((expected* expected))
           (guard (ex (else (vector #f 'exception-raised expected* ex)))
             (let ((actual* actual))
               (if (equal? expected* actual*)
                 (vector #t)
                 (vector #f 'unexpected-result expected* actual*)))))))

))
(import (ex))
(display ((check (+ 2 3) (- 13 7))))
(newline)

; -----------------------------------------------------------------------------
; from @sts-q (#31)

(define (lf) (newline))
(define (spc) (display " "))
(define (print x) (display x) (spc))

(lf) (print "-------  issue-macro.scm")
(lf)

(define-syntax amac
   (syntax-rules ()
      ((_ x ...)
         (begin
            (begin (print "<") (print 'x) (print "> "))
            ...
            #t
            ))))

(define (__amac)
   (lf) (print "__amac start")
   (lf) (amac)
   (lf) (amac 1)
   (lf) (amac 1 2)
   (lf) (amac 1 2 3)
   (lf) (print "__amac done")
   )

(__amac)
(lf)

; -----------------------------------------------------------------------------
(define-syntax amac
   (syntax-rules ()
      ((_ x ...)
         (begin
            (cond
               ((eq? x 'a)      (print "a"))
               (else            (print "?"))
               )
            ...
            #t
            ))))

;-------
(amac 1)     ; should print "?", right?
             ; what i get is
             ; Error: when calling eq?, needs 2 argument(s): (1)
             ; #0 in __amac at /home/heiko/q/scheme-tr7-local/scm/issue-macro.scm:0 args ()
(lf)

; -----------------------------------------------------------------------------

