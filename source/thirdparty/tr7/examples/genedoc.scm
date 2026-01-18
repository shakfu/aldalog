;;; generation of the documentation

(define automate-pass '(
   (init #t
      (else init))))

(define automate-std '(
   (init #f
      (#\/  init-slash)
      (else init))
   (init-slash #f
      (#\*  doc-start)
      (else init))
   (doc-start #f
      (#\newline doc0)
      (else      doc-start))
   (doc0 #f
      (#\*       doc1)
      (else      doc #f))
   (doc1 #f
      (#\/       doc-end "~~~~~~~~~~~~~~~ C\n")
      (#\space   doc)
      (#\newline doc0 "\n")
      (else      doc "*" #f))
   (doc #t
      (#\newline doc0)
      (else      doc))
   (doc-end #f
      (#\newline code0)
      (else      doc-end))
   (code0 #f
      (#\/       uncode)
      (#\newline code0 #f)
      (else      code #f))
   (code #t
      (#\newline code0)
      (else      code))
   (uncode #f
      (#\*       doc-start "~~~~~~~~~~~~~~~~~\n")
      (else      code "/" #f))
))

(define (print-state? auto state)
   (cadr (assq state auto)))

(define (next-state auto state char)
   (let* ((item (assq state auto))
          (inst (or (assq char item) (assq 'else item)))
          (next (cadr inst))
          (prin (cddr inst)))
      (for-each (lambda (x) (write-string (or x (string char)))) prin)
      next))
 
(define (process-auto auto)
   (do ((char  (read-char) (read-char))
        (state 'init       (next-state auto state char)))
      ((eof-object? char))
      (if (print-state? auto state)
         (write-char char))))

(define (prelude)
   (write-string "<meta charset='utf-8'>
<link rel='stylesheet' href='https://casual-effects.com/markdeep/latest/slate.css?'>
<style>
.md .longToc { font-size: 70%; width: 230px; }
body { left: 260px; }
</style>\n"))

(define (postlude)
   (write-string "<!-- Markdeep: -->
<script>markdeepOptions={tocStyle:'long' definitionStyle:'long', linkAPIDefinitions: true, inlineCodeLang: 'PyxlScript'};</script>
<script src='https://casual-effects.com/markdeep/latest/markdeep.min.js?'></script>\n"))

(define (process-std)
   (process-auto automate-std))

(define (process-pass)
   (process-auto automate-pass))

(define (process-std-file file)
   (define flen (string-length file))
   (define (has-ext? ext)
      (define elen (string-length ext))
      (and (<= elen flen) (string=? ext (string-copy file (- flen elen)))))
   (define (has-one-ext? . exts)
      (let next ((exts exts))
         (and (not (null? exts))
              (or (has-ext? (car exts)) (next (cdr exts))))))
   (define processor
      (cond
        ((has-one-ext? ".md" ".txt") process-pass)
        ((has-one-ext? ".c" ".h") process-std)
        (else process-pass)))
   (with-input-from-file file processor))

(define (process files)
   (prelude)
   (if (null? files)
      (process-std)
      (for-each process-std-file files))
   (postlude))

;(import (tr7 trace))(tr7-tracing 1)
(cond-expand
   ((library (scheme process-context))
      (import (only (scheme process-context) command-line))
      (process (cdr (command-line))))
   (else
      (process '())))


