; This program reads th files "r7names" and "r5names"
; and computes the text of the library definition
; for the standard R7RS library (scheme r5rs)

(import
  (scheme base)
  (scheme char)
  (scheme file)
  (scheme write))

; strip the string: removes front and back spaces
(define (string-strip str)
  (let loop ((lo 0) (up (string-length str)))
    (cond
      ; is empty?
      ((= lo up)
           "")
      ; starts with a space
      ((char-whitespace? (string-ref str lo))
           (loop (+ lo 1)    up))
      ; ends with a space
      ((char-whitespace? (string-ref str (- up 1)))
           (loop    lo    (- up 1)))
      ; is intact?
      ((and (zero? lo) (= up (string-length str)))
           str)
      ; get the computed substring
      (else
           (string-copy str lo up)))))

; for each line of the file of given "path"
; call the procedure "proc" with two parameters
; (line acc) the read line and init on first call
; the result of the last invocation of proc
; returns the result of last invocation to proc
(define (for-each-line-from-file path init proc)
   (call-with-input-file path
      (lambda (port)
         (let loop ((resu init))
            (let ((line (read-line port)))
               (if (eof-object? line)
                  (begin
                     (close-input-port port)
                     resu)
                  (let ((line (string-strip line)))
                      (loop (if (zero? (string-length line))
                                resu
                                (proc line resu))))))))))

; reads the r7 names
(define (process-r7 line accu)
   (if (eqv? #\( (string-ref line 0))
      (cons line accu)
      (cons (string->symbol line) accu)))
(define r7 (for-each-line-from-file "r7names" () process-r7))

; reads the r5 names
(define (process-r5 line accu)
   (cons (string->symbol line) accu))
(define r5 (for-each-line-from-file "r5names" () process-r5))

(define ren '((inexact . exact->inexact) (exact . inexact->exact)))

; creates the list of imports of the form
; (("(lib...)" names...) ...)
(define impo
  (let loop (
       (it r7)  ; iterator on r7
       (in ())  ; current import names
       (full #t) ; is full import?
       (resu ())) ; current result
    (if (null? it)
       resu ; terminated
       (let ((head (car it))
             (tail (cdr it)))
         (if (string? head)
           ; case of library name
           (loop tail () #t (if (null? in)
                                 resu
                                 `((,head ,full . ,in) . ,resu)))
           ; case of a symbol
           (let ((r5n (cond
                          ((assq head ren) => cdr)
                          (else head))))
              (if (memq r5n r5)
                  (loop tail (cons head in) full resu)
                  (loop tail in #f resu))))))))

(define (display-scheme-r5rs-code with-cond-expand)
  (display "(define-library (scheme r5rs)\n")
  (for-each
     (lambda (lib)
        (when with-cond-expand
           (display " (cond-expand ((library ")
           (display (car lib))
           (display ")\n")
        )
        (if (cadr lib)
           (begin
              (display "  (import ")
              (display (car lib)))
           (begin
              (display "  (import (only ")
              (display (car lib))
              (for-each
                 (lambda (sym)
                    (display " ")
                    (display sym))
                 (cddr lib))
              (display ")")))
        (display ")\n")
        (display "   (export ")
        (for-each
           (lambda (sym)
              (display " ")
              (let ((q (assq sym ren)))
                 (if q
                    (begin
                       (display "(rename ")
                       (display sym)
                       (display " ")
                       (display (cdr q))
                       (display ")"))
                    (display sym))))
           (cddr lib))
        (when with-cond-expand
           (display "))"))
        (display ")\n"))
     impo)
  (display ")\n"))

(define (make-scheme-r5rs-code with-cond-expand)
   (parameterize
     ((current-output-port (open-output-string)))
     (display-scheme-r5rs-code with-cond-expand)
     (get-output-string (current-output-port))))

(display (make-scheme-r5rs-code #f))

