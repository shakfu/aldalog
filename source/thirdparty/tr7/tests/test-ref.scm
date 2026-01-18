;#! ./tr7 -1
;
; vim: noai ts=3 sw=3 expandtab
(import (tr7 trace) (tr7 gc)
   (scheme base)
   (scheme case-lambda)
   (scheme char)
   (scheme cxr)
   (scheme inexact)
   (scheme lazy))
;(tr7-tracing 1)
(tr7-show-eval #t)
;(tr7-show-compile #t)
(tr7-show-result #t)
(tr7-show-prompt #t)
;;;; section 2.1 and 2.2

TOTO

(define |two words| "due parole")
|two words|
|two\x20;words|

#|
  The FACT procedure computes the factorial
  of a non-negative integer.
|#
(define fact
   (lambda (n)
      (if (= n 0)
          1            ;Base case: return 1
          (* n (fact (- n 1))))))

(fact 5)
(FACT 5)
#!fold-case
(FACT 5)
(fact 5)
#!no-fold-case
(FACT 5)
(fact 5)

(let ((x (list 'a 'b 'c)))
   (set-cdr! (cddr x) x)
   x)

'#1=(a b c . #1#)

'(#1=(a b) #53=(5 3) #53# #1# (#1# #53#))

;;;; section 4.1.1

(define x 28)
x

;;;; section 4.1.2

(quote a)
(quote #(a b c))
(quote (+ 1 2))

'a
'#(a b c)
'()
'(+ 1 2)
'(quote a)
''a

'145932
145932
'"abc"
"abc"
;todo;'#
;todo;#
;todo;'#(a 10)
#(a 10)
;todo;'#u8(64 65)
;todo;#u8(64 65)
'#t
#t

;;;; section 4.1.3

(+ 3 4)
((if #f + *) 3 4)

;;;; section 4.1.4

(lambda (x) (+ x x))
((lambda (x) (+ x x)) 4)

(define reverse-subtract
   (lambda (x y) (- y x)))
(reverse-subtract 7 10)

(define add4
   (let ((x 4))
      (lambda (y) (+ x y))))
(add4 6)

((lambda x x) 3 4 5 6)
((lambda (x y . z) z) 3 4 5 6)
((lambda x (list 'hello . x)) 3 4 5 6)

;;;; section 4.1.5

(if (> 3 2) 'yes 'no)
(if (> 2 3) 'yes 'no)
(if (> 3 2)
   (- 3 2)
   (+ 3 2))

;;;; section 4.1.6

(define x 2)
(+ x 1)
(set! x 4)
(+ x 1)

;;;; section 4.1.7

;todo;(include "inc1.scm")
;todo;(include-ci "inc1.scm")


;;;; section 4.2.1

(cond ((> 3 2) 'greater)
      ((< 3 2) 'less))

(cond ((> 3 3) 'greater)
      ((< 3 3) 'less)
      (else 'equal))

(cond ((assv 'b '((a 1) (b 2))) => cadr)
      (else #f))

(case (* 2 3)
      ((2 3 5 7) 'prime)
      ((1 4 6 8 9) 'composite))

(case (car '(c d))
        ((a) 'a)
        ((b) 'b))

(case (car '(c d))
      ((a e i o u) 'vowel)
      ((w y) 'semivowel)
      (else => (lambda (x) x)))

(and (= 2 2) (> 2 1))
(and (= 2 2) (< 2 1))
(and 1 2 'c '(f g))
(and)

(or (= 2 2) (> 2 1))
(or (= 2 2) (< 2 1))
(or #f #f #f)
(or (memq 'b '(a b c))
       (/ 3 0))
(or)

(when (= 1 1.0)
   (display "1")
   (display "2"))

(unless (= 1 1.0)
   (display "1")
   (display "2"))

;todo;cond-expand

;;;; section 4.2.2

(let ((x 2) (y 3))
      (* x y))

(let ((x 2) (y 3))
    (let ((x 7)
          (z (+ x y)))
       (* z x)))

(let ((x 2) (y 3))
    (let* ((x 7)
           (z (+ x y)))
       (* z x)))

(letrec ((even?
            (lambda (n)
                 (if (zero? n)
                    #t
                    (odd? (- n 1)))))
         (odd?
            (lambda (n)
                 (if (zero? n)
                    #f
                    (even? (- n 1))))))
       (even? 88))

(letrec* ((p
            (lambda (x)
               (+ 1 (q (- x 1)))))
          (q
            (lambda (y)
              (if (zero? y)
                 0
                 (+ 1 (p (- y 1))))))
          (x (p 5))
          (y x))
       y)

(define-syntax values->list
   (syntax-rules ()
     ((values->list expr)
         (call-with-values (lambda () expr) list))))

(let again ((i 0))
   (write (values->list (exact-integer-sqrt i)))
   (newline)
   (unless (> i 20) (again (+ i 1))))

(values->list (exact-integer-sqrt 50000))

(let-values (((root rem) (exact-integer-sqrt 32)))
        (* root rem))

(let-values ((a (values 1 2 3 4))) a)
(let-values (((a . b) (values 1 2 3 4))) (values a b))

(let () (define-values a (values 1 2 3 4)) a)
(let () (define-values (a . b) (values 1 2 3 4)) (values a b))

(let ((a 'a) (b 'b) (x 'x) (y 'y))
    (let*-values (((a b) (values x y))
                  ((x y) (values a b)))
          (list a b x y)))

;;;; section 4.2.3

(define x 0)
(and (= x 0)
     (begin (set! x 5)
            (+ x 1)))

(begin (display "4 plus 1 equals ")
       (display (+ 4 1))
       (display "\n"))

;;;; section 4.2.4

(do ((vec (make-vector 5))
     (i 0 (+ i 1)))
    ((= i 5) vec)
   (vector-set! vec i i))

(let ((x '(1 3 5 7 9)))
   (do ((x   x (cdr x))
        (sum 0 (+ sum (car x))))
       ((null? x) sum)))

(let loop ((numbers '(3 -2 1 6 -5))
           (nonneg '())
           (neg '()))
   (cond ((null? numbers) (list nonneg neg))
         ((>= (car numbers) 0)
              (loop (cdr numbers)
                    (cons (car numbers) nonneg)
                    neg))
         ((< (car numbers) 0)
              (loop (cdr numbers)
                    nonneg
                    (cons (car numbers) neg)))))

;;;; section 4.2.5

(force (delay (+ 1 2)))
(let ((p (delay (+ 1 2))))
     (list (force p) (force p)))

(define integers
   (letrec ((next
        (lambda (n)
            (delay (cons n (next (+ n 1)))))))
      (next 0)))
(define head
   (lambda (stream) (car (force stream))))
(define tail
   (lambda (stream) (cdr (force stream))))
(head (tail (tail integers)))

(define (stream-filter p? s)
   (delay-force
      (if (null? (force s))
         (delay '())
         (let ((h (car (force s)))
               (t (cdr (force s))))
            (if (p? h)
               (delay (cons h (stream-filter p? t)))
               (stream-filter p? t))))))

(head (tail (tail (stream-filter odd? integers))))

(define count 0)
(define p
       (delay (begin (set! count (+ count 1))
                     (if (> count x)
                           count
                           (force p)))))
(define x 5)
p
(force p)
p
(begin (set! x 10)
       (force p))

;;;; section 4.2.6

(define radix
   (make-parameter
      10
      (lambda (x)
         (if (and (exact-integer? x) (<= 2 x 16))
            x
            (error "invalid radix")))))
radix
(radix)
(define (f n) (number->string n (radix)))
(f 12)
(parameterize
   ((radix 2))
   (f 12))
(radix)
(f 12)
(radix 16)
(radix)
(f 12)
(parameterize
   ((radix 0))
   (f 12))
(radix)

(let ((param0 (make-parameter 10))
      (param1 (make-parameter 10))
      (param2 (make-parameter 10)))
   (let ((f (lambda (n) (list
                  (number->string n (param0))
                  (number->string n (param1))
                  (number->string n (param2))))))
      (display (f 151))
      (newline)
      (display (parameterize ((param0 2) (param1 8) (param2 16)) (f 151)))
      (newline)
      (display (f 151))
      (newline)))

;;;; section 4.2.7

(guard (condition
          ((assq 'a condition) => cdr)
          ((assq 'b condition)))
   (raise (list (cons 'a 42))))

(eqv? (delay 1) 1)
(pair? (delay (cons 1 2)))

;todo;bug;(+ (delay (* 3 7)) 13)
(car (list (delay (* 3 7)) 13))

(promise? x)
(promise? p)
(define q (make-promise 5))
(promise? q)
(force q)

;;;; section 4.2.8

`(list ,(+ 1 2) 4)
(let ((name 'a)) `(list ,name ',name))
`(a ,(+ 1 2) ,@(map abs '(4 -5 6)) b)
`(( foo ,(- 10 3)) ,@(cdr '(c)) . ,(car '(cons)))
`#(10 5 ,(sqrt 4) ,@(map sqrt '(16 9)) 8)
(let ((foo '(foo bar)) (@baz 'baz)) `(list ,@foo , @baz))

`(a `(b ,(+ 1 2) ,(foo ,(+ 1 3) d) e) f)
(let ((name1 'x) (name2 'y)) `(a `(b ,,name1 ,',name2 d) e))

(let ((a 3)) `((1 2) ,a ,4 ,'five 6))

(let ((a 3)) (cons '(1 2) (cons a (cons 4 (cons 'five '(6))))))


(quasiquote (list (unquote (+ 1 2)) 4))
'(quasiquote (list (unquote (+ 1 2)) 4))

(let ((a 'x))
  `(a ,a
  `(a ,a ,,a
  `(a ,a ,,a ,,,a
  `(a ,a ,,a ,,,a ,,,,a
  `(a ,a ,,a ,,,a ,,,,a ,,,,,a
  `(a ,a ,,a ,,,a ,,,,a ,,,,,a ,,,,,,a)
    a ,a ,,a ,,,a ,,,,a ,,,,,a)
    a ,a ,,a ,,,a ,,,,a)
    a ,a ,,a ,,,a)
    a ,a ,,a)
    a ,a))

;;;; section 4.2.9

(define range
   (case-lambda
      ((e) (range 0 e))
      ((b e) (do ((r '() (cons e r))
                  (e (- e 1) (- e 1)))
               ((< e b) r)))))
(range 3)
(range 3 5)

;;;; section 4.3.1


(let-syntax ((given-that (syntax-rules ()
                ((given-that test stmt)
                     (if test stmt))
                ((given-that test stmt1 stmt2 ...)
                     (if test
                         (begin stmt1
                                stmt2 ...))))))
     (let ((if #t))
        (given-that if (set! if 'now))
        if))

(let ((x 'outer))
   (let-syntax ((m (syntax-rules () ((m) x))))
      (let ((x 'inner))
          (m))))

(letrec-syntax
    ((my-or (syntax-rules ()
                   ((my-or) #f)
                   ((my-or e) e)
                   ((my-or e1 e2 ...)
                       (let ((temp e1))
                          (if temp
                              temp
                              (my-or e2 ...)))))))
   (let ((x #f)
         (y 7)
         (temp 8)
         (let odd?)
         (if even?))
     (my-or x
            (let temp)
            (if y)
            y)))

;;;; section 4.3.2

(define-syntax be-like-begin
   (syntax-rules ()
      ((be-like-begin name)
         (define-syntax name
            (syntax-rules ()
               ((name expr (... ...))
                   (begin expr (... ...))))))))

(be-like-begin sequence)
(sequence 1 2 3 4)

(let-syntax ((simple-let
                (syntax-rules ()
                   ((_ (head ... ((x . y) val) . tail) body1 body2 ...)
                       (syntax-error "expected an identifier but got" (x . y)))
                   ((_ ((name val) ...) body1 body2 ...)
                       ((lambda (name ...) body1 body2 ...) val ...)))))
    (simple-let (z ((2 3) 4) 9) body)) 

;;;; section 5.3

(define add3
   (lambda (x) (+ x 3)))
(add3 3)

(define first car)
(first '(1 2))

(let ((x 5))
  (define foo (lambda (y) (bar x y)))
  (define bar (lambda (a b) (+ (* a b) a)))
  (foo (+ x 3)))


(define-values (x y) (exact-integer-sqrt 17))
(list x y)

(let ()
  (define-values (x y) (values 1 2))
  (+ x y))


;;;; section 5.5

(define-record-type <pare>
   (kons x y)
   pare?
   (x kar set-kar!)
   (y kdr))

(pare? (kons 1 2))
(pare? (cons 1 2))
(kar (kons 1 2))
(kdr (kons 1 2))
(let ((k (kons 1 2)))
   (set-kar! k 3)
   (kar k))

;;;; section 6.1 - equivalence predicates

(eqv? 'a 'a)
(eqv? 'a 'b)
(eqv? 2 2)
; TODO: (eqv? 2 2.0)
(eqv? '() '())
(eqv? 100000000 100000000)
(eqv? 0.0 +nan.0)
(eqv? (cons 1 2) (cons 1 2))
(eqv? (lambda () 1)  (lambda () 2))
(let ((p (lambda (x) x))) (eqv? p p))
(eqv? #f '())

(eqv? "" "")
(eqv? '#() '#())
(eqv? (lambda (x) x)
   (lambda (x) x))
(eqv? (lambda (x) x)
   (lambda (y) y))
; TODO: (eqv? 1.0e0 1.0f0)
(eqv? +nan.0 +nan.0)

(define gen-counter
   (lambda ()
      (let ((n 0))
         (lambda () (set! n (+ n 1)) n))))

(let ((g (gen-counter))) (eqv? g g))
(eqv? (gen-counter) (gen-counter))

(define gen-loser
   (lambda ()
      (let ((n 0))
         (lambda () (set! n (+ n 1)) 27))))

(let ((g (gen-loser))) (eqv? g g))
(eqv? (gen-loser) (gen-loser))

(letrec ((f (lambda () (if (eqv? f g) 'f 'both)))
         (g (lambda () (if (eqv? f g) 'g 'both))))
      (eqv? f g))

(eqv? '(a) '(a))
(eqv? "a" "a")
(eqv? '(b) (cdr '(a b)))
(let ((x '(a))) (eqv? x x))

(eq?  'a 'a)
(eq?  '(a) '(a))
(eq?  (list 'a) (list 'a))
(eq?  "a" "a")
(eq?  "" "")
(eq?  '() '())
(eq?  2 2)
(eq?  #\A #\A)
(eq?  car car)
(let ((n (+ 2 3))) (eq? n n))
(let ((x '(a))) (eq? x x))
(let ((x '#())) (eq? x x))
(let ((p (lambda (x) x))) (eq? p p))

(equal? 'a 'a)
(equal? '(a) '(a))
(equal? '(a (b) c) '(a (b) c))
(equal? "abc" "abc")
(equal? 2 2)
(equal? (make-vector 5 'a) (make-vector 5 'a))
; TODO: (equal? '#1=(a b . #1#) '#2=(a b a b . #2#))
(let ((u '(a b)) (v '(a b a b))) (set-cdr! (cdr u) u) (set-cdr! (cdddr v) v) (equal? u v))
(equal? (lambda (x) x) (lambda (y) y))

(letrec ((u `(,x ,y c)) (v `(,x ,y c)) (x `#(a b ,u ,v)) (y `(a b ,u ,v)))
   (equal? u v))

;;;; section 6.2.6

;(complex? 3+4i)	;#t
(complex? 3)		;#t
(real? 3)		;#t
;(real? -2.5+0i)	;#t
;(real? -2.5+0.0i)	;#f
;(real? #e1e10)		;#t
(real? +inf.0)		;#t
(real? +nan.0)		;#t
(rational? -inf.0)	;#f
(rational? 3.5)		;#t
;(rational? 6/10)	;#t
;(rational? 6/3)	;#t
;(integer? 3+0i)	;#t
(integer? 3.0)		;#t
;(integer? 8/4)		;#t

(exact? 3.0)		;#f
;(exact? #e3.0)		;#t
(inexact? 3.)		;#t

(exact-integer? 32)	;#t
(exact-integer? 32.0)	;#f
;(exact-integer? 32/5)	;#f

(finite? 3)		;#t
(finite? +inf.0)	;#f
;(finite? 3.0+inf.0i)	;#f

(infinite? 3)		;#f
(infinite? +inf.0)	;#t
(infinite? +nan.0)	;#f
;(infinite? 3.0+inf.0i)	;#t

(nan? +nan.0)		;#t
(nan? 32)		;#f
;(nan? +nan.0+0.5i)	;#t
;(nan? 1+2i)		;#f

(= 1 2)
(= 1 1.0)
(= 1.2 1.0)

(<= 1 2)
(<= 1 1.0)
(<= 1.2 1.0)

(>= 1 2)
(>= 1 1.0)
(>= 1.2 1.0)

(< 1 2)
(< 1 1.0)
(< 1.2 1.0)

(> 1 2)
(> 1 1.0)
(> 1.2 1.0)

(= +0.0 -0.0)
(zero? 4)
(zero? 0)
(zero? -4)
(zero? 4.0)
(zero? 0.0)
(zero? -4.0)
(zero? +0.0)
(zero? -0.0)
(positive? 4)
(positive? 0)
(positive? -4)
(positive? 4.0)
(positive? 0.0)
(positive? -4.0)
(positive? +0.0)
(positive? -0.0)
(negative? 4)
(negative? 0)
(negative? -4)
(negative? 4.0)
(negative? 0.0)
(negative? -4.0)
(negative? +0.0)
(negative? -0.0)

(max 3 4 2)
(max 3.9 4 2)
(min 3 4 2)
(min 3.9 4 2)

(- 3 4)
(- 3 4 5)
(- 3)
(/ 3 4 5)
(/ 3)

(abs -7)

(values->list (floor/ 5 2))
(values->list (floor/ -5 2))
(values->list (floor/ 5 -2))
(values->list (floor/ -5 -2))
(values->list (truncate/ 5 2))
(values->list (truncate/ -5 2))
(values->list (truncate/ 5 -2))
(values->list (truncate/ -5 -2))
(values->list (truncate/ -5.0 -2))

(gcd 32 -36)
(gcd)
(lcm 32 -36)
(lcm 32.0 -36)
(lcm)

(floor -4.3)
(ceiling -4.3)
(truncate -4.3)
(round -4.3)

(floor 3.5)
(ceiling 3.5)
(truncate 3.5)
(round 3.5)

;(round 7/2)
(round 7)


(square 7)
(square -1.1)

(values->list (exact-integer-sqrt 4))
(values->list (exact-integer-sqrt 5))


;;;; section 6.3 - booleans

#t
#f
'#f

(not #t)
(not 3)
(not (list 3))
(not #f)
(not '())
(not (list))
(not 'nil)

(boolean? #f)
(boolean? 0)
(boolean? '())

(boolean=? 1 1 1)
(boolean=? #f #f #f)
(boolean=? #t #t #t)
(boolean=? #f #f #t)

;;;; section 6.4 - pairs and lists

(pair? '(a . b))
(pair? '(a b c))
(pair? '())
(pair? '#(a b))

(cons 'a '())
(cons '(a) '(b c d))
(cons "a" '(b c))
(cons 'a 3)
(cons '(a b) 'c)

(car '(a b c))
(car '((a) b c d))
(car '(1 . 2))
(car '())

(cdr '((a) b c d))
(cdr '(1 . 2))
(cdr '())

(car '(car . cdr))
(cdr '(car . cdr))

(caar '((caar . cdar) cadr . cddr))
(cadr '((caar . cdar) cadr . cddr))
(cdar '((caar . cdar) cadr . cddr))
(cddr '((caar . cdar) cadr . cddr))

(caaar '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))
(caadr '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))
(cadar '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))
(caddr '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))
(cdaar '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))
(cdadr '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))
(cddar '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))
(cdddr '(((caaar . cdaar) cadar . cddar) (caadr . cdadr) caddr . cdddr))

(caaaar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(caaadr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(caadar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(caaddr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cadaar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cadadr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(caddar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cadddr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cdaaar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cdaadr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cdadar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cdaddr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cddaar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cddadr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cdddar '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))
(cddddr '((((caaaar . cdaaar) cadaar . cddaar) (caadar . cdadar) caddar . cdddar) ((caaadr . cdaadr) cadadr . cddadr) (caaddr . cdaddr) cadddr . cddddr))

(list? '(a b c))
(list? '())
(list? '(a . b))
(let ((x (list 'a)))
   (set-cdr! x x)
   (list? x))

(make-list 2 3)

(list 'a (+ 3 4) 'c)
(list)

(length '(a b c))
(length '(a (b) (c d e)))
(length '())

(append '(x) '(y))
(append '(a) '(b c d))
(append '(a (b)) '((c)))
(append '(a b) '(c . d))
(append '() 'a)
(let* ((a '(a b c))
       (d '(d e f))
       (u (append a d)))
   (set-cdr! a 1)
   (set-cdr! d 4)
   u)

(reverse '(a b c))
(reverse '(a (b c) d (e (f))))

(list-ref '(a b c d) 2)
(list-ref '(a b c d)
   (exact (round 1.8)))
 
(let ((ls (list 'one 'two 'five!)))
   (list-set! ls 2 'three)
   ls)

(memq 'a '(a b c))
(memq 'b '(a b c))
(memq 'a '(b c d))
(memq (list 'a) '(b (a) c))
(member (list 'a) '(b (a) c))
(member "B" '("a" "b" "c") string-ci=?)
(memq 101 '(100 101 102))
(memv 101 '(100 101 102))

(assq 'a '((a 1) (b 2) (c 3)))
(assq 'b '((a 1) (b 2) (c 3)))
(assq 'd '((a 1) (b 2) (c 3)))
(assq (list 'a) '(((a)) ((b)) ((c))))
(assoc (list 'a) '(((a)) ((b)) ((c))))
(assoc 2.0 '((1 1) (2 4) (3 9)) =)
(assq 5 '((2 3) (5 7) (11 13)))
(assv 5 '((2 3) (5 7) (11 13)))

(list-copy 7)
(list-copy '())
(list-copy #u8(0 0 0 0))
(list-copy '(a b 1 2 3))

(let* ((a '(1 8 2 8))) (eq? a (list-copy a)))
(let* ((a '(1 8 2 8))) (eqv? a (list-copy a)))
(let* ((a '(1 8 2 8))) (equal? a (list-copy a)))

(let* ((a '(1 8 2 8))
       (b (list-copy a)))
   (set-car! b 3)
   b)

(let ((v '(a b a b)))
   (set-cdr! (cdddr v) v)
   (list-copy v))

;;;; section 6.5 - symbols

(symbol? 'foo)
(symbol? (car '(a b)))
(symbol? "bar")
(symbol? 'nil)
(symbol? '())
(symbol? #f)

(symbol=? 'a 'a (string->symbol "a"))
(symbol=? 'a 'a (string->symbol "a") 'h)

(symbol->string 'flying-fish)
(symbol->string 'Martin)
(symbol->string (string->symbol "Malvina"))
(string->symbol "mISSISSIppi")
(eqv? 'bitBlt (string->symbol "bitBlt"))
(eqv? 'LollyPop (string->symbol (symbol->string 'LollyPop)))
(string=? "K. Harper, M.D." (symbol->string (string->symbol "K. Harper, M.D.")))

;;;; section 6.6 - characters

#\alarm
#\x07
#\backspace
#\x08
#\delete
#\x7f
#\escape
#\x1b
#\newline
#\x0a
#\null
#\x00
#\return
#\x0d
#\space
#\x20
#\tab
#\x09

#\a
#\A
#\(
#\ 

(char? #t)
(char? #\a)

(char=? #\z #\z #\z) 
(char=? #\z #\Z #\z) 
(char=? #\z #\z #\x) 

(char>? #\z #\y #\x) 
(char>? #\Z #\y #\x) 
(char>? #\z #\y #\y #\x) 
(char>? #\a #\b #\c) 
(char>? #\a #\B #\c) 
(char>? #\a #\b #\b #\c) 

(char<? #\z #\y #\x) 
(char<? #\Z #\y #\x) 
(char<? #\z #\y #\y #\x) 
(char<? #\a #\b #\c) 
(char<? #\a #\B #\c) 
(char<? #\a #\b #\b #\c) 

(char>=? #\z #\y #\x) 
(char>=? #\Z #\y #\x) 
(char>=? #\z #\y #\y #\x) 
(char>=? #\a #\b #\c) 
(char>=? #\a #\B #\c) 
(char>=? #\a #\b #\b #\c) 

(char<=? #\z #\y #\x) 
(char<=? #\Z #\y #\x) 
(char<=? #\z #\y #\y #\x) 
(char<=? #\a #\b #\c) 
(char<=? #\a #\B #\c) 
(char<=? #\a #\b #\b #\c) 

(char-ci=? #\z #\z #\z) 
(char-ci=? #\z #\Z #\z) 
(char-ci=? #\z #\z #\x) 

(char-ci>? #\z #\y #\x) 
(char-ci>? #\Z #\y #\x) 
(char-ci>? #\z #\y #\y #\x) 
(char-ci>? #\a #\b #\c) 
(char-ci>? #\a #\B #\c) 
(char-ci>? #\a #\b #\b #\c) 

(char-ci<? #\z #\y #\x) 
(char-ci<? #\Z #\y #\x) 
(char-ci<? #\z #\y #\y #\x) 
(char-ci<? #\a #\b #\c) 
(char-ci<? #\a #\B #\c) 
(char-ci<? #\a #\b #\b #\c) 

(char-ci>=? #\z #\y #\x) 
(char-ci>=? #\Z #\y #\x) 
(char-ci>=? #\z #\y #\y #\x) 
(char-ci>=? #\a #\b #\c) 
(char-ci>=? #\a #\B #\c) 
(char-ci>=? #\a #\b #\b #\c) 

(char-ci<=? #\z #\y #\x) 
(char-ci<=? #\Z #\y #\x) 
(char-ci<=? #\z #\y #\y #\x) 
(char-ci<=? #\a #\b #\c) 
(char-ci<=? #\a #\B #\c) 
(char-ci<=? #\a #\b #\b #\c) 

(char-alphabetic? #\a)
(char-alphabetic? #\B)
(char-alphabetic? #\5)
(char-alphabetic? #\,)
(char-alphabetic? #\tab)

(char-numeric? #\a)
(char-numeric? #\B)
(char-numeric? #\5)
(char-numeric? #\,)
(char-numeric? #\tab)

(char-whitespace? #\a)
(char-whitespace? #\B)
(char-whitespace? #\5)
(char-whitespace? #\,)
(char-whitespace? #\tab)

(char-upper-case? #\a)
(char-upper-case? #\B)
(char-upper-case? #\5)
(char-upper-case? #\,)
(char-upper-case? #\tab)

(char-lower-case? #\a)
(char-lower-case? #\B)
(char-lower-case? #\5)
(char-lower-case? #\,)
(char-lower-case? #\tab)

(digit-value #\3)
(digit-value #\e)

(char->integer #\3)
(char->integer #\e)

(integer->char 51)
(integer->char 101)

;;;; section 6.7

"The word \"recursion\" has many meanings."
"Another example:\ntwo lines of text"
"Here’s text \
   containing just one line"
"\x03B1; is named GREEK SMALL LETTER ALPHA."

(string? "hello")
(string? 5)

(make-string 6)
(make-string 10 #\X)

(string #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7)
(string)

(string-length "")
(string-length "test")

(string-ref "0123456789" 5)
(let ((s (string-copy "0123456789"))) (string-set! s 5 #\X) s)

(string=? "123" "123" "123" "123")
(string=? "123" "123" "123" "456")
(string<=? "123" "123" "123" "456")

(string-upcase "Hello")
(string-upcase "ööö")
(string-downcase "Hello")
(string-downcase "ÖÖÖ")
(string-foldcase "Hello")

(string-append "123" "456" "789")
(string->list "Hello")
(string->list "Hello" 2)
(string->list "Hello" 2 4)

(list->string (list #\H #\e #\l #\l #\o #\H #\e #\l #\l #\o))

(string-copy "0123456789")
(string-copy "0123456789" 3)
(string-copy "0123456789" 3 8)
(string-copy "0123456789" 10 10)

(let ((s (string-copy "0123456789"))) (string-copy! s 3 "ABCD") s)
(let ((s (string-copy "0123456789"))) (string-copy! s 3 "ABCD" 2) s)
(let ((s (string-copy "0123456789"))) (string-copy! s 3 "ABCD" 1 3) s)
(let ((s (string-copy "0123456789"))) (string-copy! s 3 "ABCD" 3 3) s)
(let ((s (string-copy "0123456789"))) (string-copy! s 10 "ABCD" 3 3
) s)

(let ((s (string-copy "0123456789"))) (string-fill! s #\x) s)
(let ((s (string-copy "0123456789"))) (string-fill! s #\x 3) s)
(let ((s (string-copy "0123456789"))) (string-fill! s #\x 3 7) s)
(let ((s (string-copy "0123456789"))) (string-fill! s #\x 10 10) s)

;;;; section 6.8 - vectors

(vector 'a 'b 'c)
(vector-ref '#(1 1 2 3 5 8 13 21) 5)

(vector-ref '#(1 1 2 3 5 8 13 21)
   (exact (round (* 2 (acos -1)))))

(let ((vec (vector 0 '(2 2 2 2) "Anna")))
   (vector-set! vec 1 '("Sue" "Sue"))
   vec)

(vector->list '#(dah dah didah))
(vector->list '#(dah dah didah) 1 2)
(list->vector '(dididit dah))

(string->vector "0123456789")
(string->vector "0123456789" 2)
(string->vector "0123456789" 2 4)

(vector->string #(#\0 #\1 #\2 #\3 #\4 #\5))
(vector->string #(#\0 #\1 #\2 #\3 #\4 #\5) 2)
(vector->string #(#\0 #\1 #\2 #\3 #\4 #\5) 2 4)

(vector-copy (string->vector "0123456789"))
(vector-copy (string->vector "0123456789") 3)
(vector-copy (string->vector "0123456789") 3 8)
(vector-copy (string->vector "0123456789") 10 10)

(let ((s (string->vector "0123456789"))) (vector-copy! s 3 #(#\a #\b #\c)) s)
(let ((s (string->vector "0123456789"))) (vector-copy! s 3 #(#\a #\b #\c) 2) s)
(let ((s (string->vector "0123456789"))) (vector-copy! s 3 #(#\a #\b #\c) 1 3) s)
(let ((s (string->vector "0123456789"))) (vector-copy! s 3 #(#\a #\b #\c) 3 3) s)
(let ((s (string->vector "0123456789"))) (vector-copy! s 10 #(#\a #\b #\c) 3 3) s)

(vector-append #(0 1 2) #(3 4 5) #(6 7 8 9 10))

(let ((a (vector 1 2 3 4 5))) (vector-fill! a 'smash) a)
(let ((a (vector 1 2 3 4 5))) (vector-fill! a 'smash 2) a)
(let ((a (vector 1 2 3 4 5))) (vector-fill! a 'smash 2 4) a)
(let ((a (vector 1 2 3 4 5))) (vector-fill! a 'smash 5 5) a)

;;;; section 6.9 - byte arrays

#u8(0 10 5)

(make-bytevector 2 12)
(make-bytevector 3)

(bytevector? #(0 10 5))
(bytevector? #u8(0 10 5))

(bytevector-length (make-bytevector 8 7))

(bytevector-u8-ref #u8(1 1 2 3 5 8 13 21) 5)

(let ((bv (bytevector 1 2 3 4)))
   (bytevector-u8-set! bv 1 3)
   bv)

(let ((a #u8(1 2 3 4 5)))
   (bytevector-copy a 2 4))

(let ((a (bytevector 1 2 3 4 5))
      (b (bytevector 10 20 30 40 50)))
   (bytevector-copy! b 1 a 0 2)
   b)

(bytevector-append #u8(0 1 2) #u8(3 4 5) #u8(6 7 8 9 10))


;;;; section 6.10

(map cadr '((a b) (d e) (g h)))
(map (lambda (n) (expt n n)) '(1 2 3 4 5))
(map + '(1 2 3) '(4 5 6 7))
(let ((count 0))
   (map (lambda (ignored)
      (set! count (+ count 1))
      count)
   '(a b)))

(string-map char-foldcase "AbdEgH")
(string-map
   (lambda (c)
      (integer->char (+ 1 (char->integer c))))
   "HAL")
(string-map
   (lambda (c k)
      ((if (eqv? k #\u) char-upcase char-downcase) c))
   "studlycaps xxx"
   "ululululul")

(string-map
   (let ((kte (- (char->integer #\A) (char->integer #\0))))
      (lambda (c)
         (integer->char (+ kte (char->integer c)))))
   "0123456789")

(string-map
   (let ((kte (- (char->integer #\x3b1) (char->integer #\0))))
      (lambda (c)
         (integer->char (+ kte (char->integer c)))))
   "0123456789")

(vector-map cadr '#((a b) (d e) (g h)))
(vector-map (lambda (n) (expt n n)) '#(1 2 3 4 5))
(vector-map + '#(1 2 3) '#(4 5 6 7))
(let ((count 0))
   (vector-map
      (lambda (ignored) (set! count (+ count 1)) count)
      '#(a b)))

(let ((v (make-vector 5)))
   (for-each (lambda (i)
         (vector-set! v i (* i i)))
      '(0 1 2 3 4))
   v)

(let ((v '()))
   (string-for-each
      (lambda (c) (set! v (cons (char->integer c) v)))
      "abcde")
   v)

(let ((v '()))
   (string-for-each
      (lambda (c) (set! v (cons (char->integer c) v)))
      "\x3b1;\x3b2;\x3b3;\x3b4;")
   v)

(string-for-each display "\x3b1;\x3b2;\x3b3;\x3b4;\n")

(let ((v (make-list 5)))
   (vector-for-each
      (lambda (i) (list-set! v i (* i i)))
      '#(0 1 2 3 4))
   v)


(call/cc
   (lambda (exit)
      (for-each
         (lambda (x) (if (negative? x) (exit x)))
         '(54 0 37 -3 245 19))
      #t))

(call-with-values (lambda () (values 4 5)) (lambda (a b) a))
(call-with-values (lambda () (values 4 5)) (lambda (a b) b))
(call-with-values * -)

(let ((path '())
      (c #f))
   (let ((add (lambda (s)
                 (set! path (cons s path)))))
      (dynamic-wind
         (lambda () (add 'connect))
         (lambda ()
            (add (call-with-current-continuation
               (lambda (c0)
                  (set! c c0)
                  'talk1))))
         (lambda () (add 'disconnect)))
      (if (< (length path) 4)
         (c 'talk2)
         (reverse path))))

;;;; section 6.11

(call-with-current-continuation
   (lambda (k)
      (with-exception-handler
          (lambda (x)
                (display "condition: ")
                (write x)
                (newline)
                (k 'exception))
          (lambda ()
                (+ 1 (raise 'an-error))))))

(with-exception-handler
   (lambda (x)
       (display "something went wrong\n"))
   (lambda ()
       (+ 1 (raise 'an-error))))

(with-exception-handler
   (lambda (con)
       (cond
          ((string? con)   (display con))
          (else            (display "a warning has been issued\n")))
       42)
   (lambda ()
       (+ (raise-continuable "should be a number\n") 23)))

;;;; section 6.13

(define (cat-port port)
   (let loop ((v (read port)))
      (unless (eof-object? v)
         (write v)
         (newline)
         (loop (read port)))))

(let ((f (open-input-file "test-ref.scm.aux")))
   (cat-port f)
   (close-port f))

(call-with-input-file "test-ref.scm.aux" cat-port)
(call-with-port (open-input-file "test-ref.scm.aux") cat-port)

(define (cat) (cat-port (current-input-port)))
(with-input-from-file "test-ref.scm.aux" cat)

(let ((rec (quote  #0=("a" b c . #0#)))
      (sha (quote (#1=("a" b) #53=(5 3) #53# #1# (#1# #53#)))) 
      (p   (lambda (fun obj)(fun obj)(newline))))
   (p write rec)
   (p write sha)
   (p write-shared rec)
   (p write-shared sha)
   (p write-simple sha)
   (p display rec)
   (p display sha))

(parameterize
  ((current-output-port
    (open-output-string)))
  (display "piece")
  (display " by piece ")
  (display "by piece.")
  (newline)
  (get-output-string (current-output-port)))

(let* ((twp (lambda (port)
                 (guard (err ((read-error? err) "read-error")
                             ((file-error? err) "file-error")
                             ( #t               "other-error"))
                     (read port))))
       (td  (lambda (port) (display (twp port))(newline))))
   (td (open-input-string ""))
   (td (open-input-string "5"))
   (td (open-input-string ")5"))
   (td (open-input-bytevector #u8(0 0)))
   )

;;;; section 6.13

(file-exists? "invalidfile")
(with-output-to-file "invalidfile"
   (lambda () (write "hello world") (display #\newline)))
(file-exists? "invalidfile")
(with-input-from-file "invalidfile" cat)
(delete-file "invalidfile")
(file-exists? "invalidfile")

;;;; EXTRA srfi 136

(define-record-type <root>
   root
   root?
   (value root-get root-set!))

(define-record-type (<kind1> <root>)
   kind1
   kind1?
   (value1 kind1-get1 kind1-set1!)
   (value2 kind1-get2))

(define-record-type (<kind2> <root>)
   kind2
   kind2?
   (value1 kind2-get1 kind2-set1!)
   (value2 kind2-get2))

(define-record-type (<kind22> <kind2>)
   kind22
   kind22?
   (value22 kind22-get))

(define r   (root     1))
(define k1  (kind1   11  12  13))
(define k2  (kind2   21  22  23))
(define k22 (kind22 221 222 223 224))
   
(root? r)
(root? k1)
(root? k2)
(root? k22)

(kind1? r)
(kind1? k1)
(kind1? k2)
(kind1? k22)

(kind2? r)
(kind2? k1)
(kind2? k2)
(kind2? k22)

(kind22? r)
(kind22? k1)
(kind22? k2)
(kind22? k22)

(root-get r)
(root-get k1)
(root-get k2)
(root-get k22)

(root-set! r   1001)
(root-set! k1  1011)
(root-set! k2  1021)
(root-set! k22 1221)

(root-get r)
(root-get k1)
(root-get k2)
(root-get k22)

(kind1-get1 k1)
(kind2-get1 k2)
(kind2-get1 k22)

(kind1-set1! k1  1012)
(kind2-set1! k2  1022)
(kind2-set1! k22 1222)

(kind1-get1 k1)
(kind2-get1 k2)
(kind2-get1 k22)

(kind1-get2 k1)
(kind2-get2 k2)
(kind2-get2 k22)
(kind22-get k22)

;;;; lambda

((λ x (for-each display x)) "hello" #\space "world!" #\newline)

#d_1_000_000
#x_f_4240
#b_1111_0100_0010_0100_0000
#b_11_110_100_001_001_000_000
#o_3_641_100

;;;; bug 29

(define plus #;(this works) (lambda (x #;(this works) y #;(this no more error on tr7)) #;(this works) (+ x y)))
(plus 4 5)

;;;; character replacements

(map char->integer (map integer->char (list 0 #xd7ff #xd800 #xdfff #xe000 #x10ffff #x110000 #xffffffff)))
(map char-unicode? (map integer->char (list 0 #xd7ff #xd800 #xdfff #xe000 #x10ffff #x110000 #xffffffff)))
(list->string (map integer->char (list 0 #xd #x41 #xe0 #xffffffff)))
(write-char (integer->char #xffffffffffff))
(string->utf8 (string (integer->char #xffffffffffff)))

;;;; built in hashing

(hash-by-identity 1)
(hash-by-identity '())
(hash-by-identity #f)
(hash-by-identity #t)
(hash-by-identity #\A)
; test of other values may differ from arch 32 bits and arch 64 bits
;(string-hash "a string" 1024)
;(symbol-hash '|a string| 1024)
;(string-ci-hash "A StRinG" 1024)

;;;; bug #43

(define-record-type foo
  (make-foo)
  foo?
  (bar bar-get bar-set!))

(define second-bar-get bar-get)
(make-foo)
(make-foo 1)

(define-record-type foo
  (make-foo bar)
  foo?
  (bar bar-get bar-set!)
  (baz baz-get baz-set!))

(define second-bar-get bar-get)
(make-foo)
(make-foo 1)
(make-foo 1 2)

;;;; bug #44

(define-syntax foo
  (syntax-rules ()
    ((_ x (l ...) y ...)
     (display '(x (l ...) : y ...)))))

(foo X () A B C)
(newline)
(foo X (u))
(newline)

;;;; bug #45

(define-syntax foo
  (syntax-rules ()
    ((_ x (l ...) y ...)
     (display '(_ (l x) ... : y ...)))))

(foo X (u))
(foo X () A B C)
(foo X (a b c) A B C)

;;;; bug

(newline)

(define-syntax foo2
  (syntax-rules ()
    ((_ (x y ...) ...) (begin (for-each display (list "x=" x ", y:" y ... "\n"))...))))

(foo2 (1 2 3 4) (5 6 7 8))

;;;; END

(display "END\n")

;(tr7-tracing 0)
(tr7-gc-verbose #t)
(tr7-gc)
