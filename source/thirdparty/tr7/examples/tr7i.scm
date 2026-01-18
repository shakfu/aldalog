; vim: noai ts=3 sw=3 expandtab

(import (scheme eval)(tr7 environment))

(define evalenv (environment '(scheme base) '(scheme read) '(scheme write)
                             '(scheme file) '(scheme load) '(scheme eval)
                             '(scheme process-context)))

(define predef (tr7-environment->list evalenv))

(define colors '(
   none    "\x1b;[0m"
   black    "\x1b;[30m"
   red      "\x1b;[31m"
   green    "\x1b;[32m"
   yellow   "\x1b;[33m"
   blue     "\x1b;[34m"
   magenta  "\x1b;[35m"
   cyan     "\x1b;[36m"
   white    "\x1b;[37m"
   gray     "\x1b;[30;1m"
   grey     "\x1b;[30;1m"
   bright_red      "\x1b;[31;1m"
   bright_green    "\x1b;[32;1m"
   bright_yellow   "\x1b;[33;1m"
   bright_blue     "\x1b;[34;1m"
   bright_magenta  "\x1b;[35;1m"
   bright_cyan     "\x1b;[36;1m"
   bright_white    "\x1b;[37;1m"
   ))

(define styles '(
   default     black
   comment     white
   string      green
   regex       cyan
   number      green
   keyword     blue
   function    gray
   type        bright_magenta
   identifier  yellow
   error       bright_red
   result      black
   error_msg   bright_red
   ))

(define (color-tag color)
   (cadr (or (memq color colors) colors)))

(define (style-color style)
   (cond ((memq style styles) => cadr)
         (else 'none)))

(define (style-tag style)
   (color-tag (style-color style)))

(define (display-color text color)
   (display (color-tag color))
   (display text)
   (display (color-tag 'none)))

(define (display-style text style)
   (display-color text (style-color style)))



(define prompt-string "tr7i> ")
(define show-eval? #f)
(define show-result? #t)
(define show-prompt? #t)

(if #t
   (display-color "tr7i version 0.1\n" 'bright_yellow))

(let repl ()
   (if show-prompt?
      (display-color prompt-string 'bright_white))
   (display (color-tag 'white))
   (let ((expr (read)))
      (unless (eof-object? expr)
         (when show-eval?
            (display expr)
            (newline))
         (let-values (((ok resu) (guard (e (#t (values #f e)))
                                    (call-with-values
                                      (lambda () (eval expr evalenv))
                                      (lambda x (values #t x))))))

            (when show-result?
               (if ok
                  (for-each (lambda (item) (write-shared item) (newline)) resu)
                  (begin
                     (if (error-object? resu)
                        (begin
                           (display "error: ")
                           (display (error-object-message resu))
                           (unless (null? (error-object-irritants resu))
                              (display " ")
                              (display (error-object-irritants resu))))
                        (display "catched unknow exception"))
                     (newline))))
            (repl)))))
(newline)
