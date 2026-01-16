(import (scheme base) (scheme case-lambda) (tr7 trace) (tr7 tagged-closures) (srfi 259))

(tr7-show-eval #t)
(tr7-show-result #t)
(tr7-show-prompt #t)

;;; Low-level tagged-closures library.

(closure? car)                         ;=> #f

(define %car (lambda args (apply car args)))
(closure? %car)                        ;=> #t
(%car '(1 . 2))                        ;=> 1
(closure-get-tag %car)                 ;=> '()
(%car '(1 . 2))                        ;=> 1
(closure-set-tag! %car 0)
(%car '(1 . 2))                        ;=> 1
(closure-get-tag %car)                 ;=> 0
(%car '(1 . 2))                        ;=> 1

(define %car2 (closure-copy %car))
(closure? %car2)                       ;=> #t
(%car2 '(1 . 2))                       ;=> 1
(closure-get-tag %car2)                ;=> 0
(%car2 '(1 . 2))                       ;=> 1
(closure-set-tag! %car2 100)
(%car2 '(1 . 2))                       ;=> 1
(closure-get-tag %car2)                ;=> 100
(closure-get-tag %car)                 ;=> 0
(closure-set-tag! %car 200)
(closure-get-tag %car)                 ;=> 200
(closure-get-tag %car2)                ;=> 100

(define %test (case-lambda
                (() 1)
                ((x) 2)))
(closure? %test)
(closure-get-tag %test)
(closure-set-tag! %test 100)
(closure-get-tag %test)
(%test)
(%test 1000)

;;; High-level SRFI 259
;;;
;;; This is mostly copy-pasted from the SRFI document:
;;;
;;; © 2025 Daphne Preston-Kendal.
;;;
;;; Permission is hereby granted, free of charge, to any person obtaining
;;; a copy of this software and associated documentation files (the
;;; "Software"), to deal in the Software without restriction, including
;;; without limitation the rights to use, copy, modify, merge, publish,
;;; distribute, sublicense, and/or sell copies of the Software, and to
;;; permit persons to whom the Software is furnished to do so, subject to
;;; the following conditions:
;;;
;;; The above copyright notice and this permission notice (including the
;;; next paragraph) shall be included in all copies or substantial
;;; portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
;;; IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY
;;; CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT,
;;; TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE
;;; SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define-procedure-tag make-a-tagged a-tagged? a-tag)
(define-syntax should-throw
  (syntax-rules ()
    ((_ body ...)
     (call/cc
      (lambda (return)
        (with-exception-handler (lambda (ex) (return 'ok))
          (lambda () body ...)))))))

(should-throw (make-a-tagged 0 #t))
(should-throw (make-a-tagged 0 '()))
(should-throw (make-a-tagged 0 0))
(should-throw (make-a-tagged 0 'call/cc))
(should-throw (make-a-tagged 0 "string"))
(should-throw (make-a-tagged 0 '(pair)))

(define (greet whom) (list 'hello whom))
(should-throw (make-a-tagged greet))

(define greet-a (make-a-tagged 12 greet))

(a-tagged? greet-a) ;=> #t
(a-tagged? greet)   ;=> #f
(a-tagged? 'a)      ;=> #f

(a-tag greet-a) ;=> 12
(should-throw (a-tag greet))
(should-throw (a-tag 'a))

(greet 'world)   ;=> (hello world)
(greet-a 'world) ;=> (hello world)

(define greet-a* (make-a-tagged 12 greet))
(eqv? greet-a greet-a*) ;=> #f
(eq? greet-a greet-a*)  ;=> #f

(greet-a* 'world) ;=> (hello world)

(define tagged-car (make-a-tagged 100 car))
(a-tagged? tagged-car)                 ;=> #t
(a-tagged? car)                        ;=> #f
(a-tag tagged-car)                     ;=> 100
(car '(1 . 2))                         ;=> 1

;;; A second procedure tag
(define-procedure-tag make-b-tagged b-tagged? b-tag)

(define greet-b (make-b-tagged 34 greet))

(a-tagged? greet-b) ;=> #f
(b-tagged? greet-b) ;=> #t

(b-tag greet-b) ;=> 34


;; Two tags on a single procedure

(define greet-ab (make-b-tagged 56 greet-a))

(a-tagged? greet-ab) ;=> #t
(b-tagged? greet-ab) ;=> #t

(a-tag greet-ab) ;=> 12
(b-tag greet-ab) ;=> 56


;; Replacing an existing tag

(define greet-ab* (make-a-tagged 1234 greet-ab))

(a-tagged? greet-ab*) ;=> #t
(b-tagged? greet-ab*) ;=> #t

(a-tag greet-ab*) ;=> 1234
(b-tag greet-ab*) ;=> 56

(a-tag greet-ab) ;=> 12


;; Define-procedure-tag is generative

(define (make-procedure-tag)
  (define-procedure-tag make is-a? ref)
  (values make is-a? ref))

(define-values (make-c-tagged c-tagged? c-tag) (make-procedure-tag))
(define-values (make-d-tagged d-tagged? d-tag) (make-procedure-tag))

(define greet-c (make-c-tagged 'alpha greet))
(define greet-d (make-d-tagged 'beta greet))

(c-tagged? greet-c) ;=> #t
(d-tagged? greet-c) ;=> #f
(c-tagged? greet-d) ;=> #f
(d-tagged? greet-d) ;=> #t

(c-tag greet-c) ;=> alpha
(d-tag greet-d) ;=> beta

(define test (make-a-tagged 100
                            (case-lambda
                              (() 1)
                              ((x) 2))))
(a-tagged? test)
(a-tag test)
(define test2 (make-a-tagged 1000 test))
(a-tagged? test2)
(a-tag test2)
(a-tag test)

