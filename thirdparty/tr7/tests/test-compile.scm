;;; testing compilation

(import (scheme write)
        (scheme eval)
        (scheme lazy)
        (scheme case-lambda)
        (tr7 debug))

(define a '<a>)
(define b '<b>)
(define c '<c>)
(define d '<d>)
(define e '<e>)
(define f '<f>)
(define u '<u>)
(define v '<v>)
(define w '<w>)
(define x '<x>)
(define y '<y>)
(define z '<z>)
(define (action x y) (list x y))
(define pass list)

(define test-cases '(

   ; reference
   a

   ; quote
   'a
   '(a b)

   ; quasiquote
   `a
   `,a
   `(,a)
   `(,a b)
   `(a ,b)
   `(a ,b c)
   `(a ,b c d)
   `(a ,@(action 1 2) c)
   `(a ,b `(,c ,,d))

   ; begin
   (begin 1)
   (begin 1 2)
   (begin 1 2 3)

   ; if then else
   (if a b)
   (if a b c)
   (if (not a) b c)
   (begin 0 (if a b c) d)

   ; unless
   (unless a b c)

   ; when
   (when a b c)

   ; or
   (or)
   (or a)
   (or a b)
   (or a b c)

   ; and
   (and)
   (and a)
   (and a b)
   (and a b c)

   ; cond
   (cond (#t 1))
   (cond (#t 1) (#f 6))
   (cond (#t 1) (else 6))
   (cond (#t 1) ((or #f #t) => x) (else 8))

   ; case
   (case a ((1 2 3) 1) ((4 5 6) 2) (else 3))
   (case a ((1 2 3) 1) ((4 <a> 6) 2) (else 3))
   (case a ((1 2 3) => x) ((4 5 6) => y) (else => pass))

   ; let
   (let () 5)
   (let ((a u)(b a)) a b)
   (let* ((a u)(b a)) a b)
   (let ((a 5)) (let ((b a)) b) a)
   (let-values (((a b) (values 5 6)))  b a)

   ; named let
   (let fact5 ((n 5)) (if (zero? n) 1 (* n (fact5 (- n 1)))))
   (let fact5 ((n 5)(r 1)) (if (zero? n) r (fact5 (- n 1) (* n r))))

   ; define
   (define a 3)
   (let () (define a 4))
   (define (1+ x) (+ x 1))

   ; define-values
   (define-values (a b) (values 5 6))
   (let () (define-values (a b) (values 7 8)))

   ; set!
   (set! a 9)
   (let () (set! a 15))
   (let ((a 1)(b 2)) (set! a 20))

   ; lambda
   (lambda () e)
   (lambda (a b) e a)
   (lambda (a . b) (lambda u (+ e a c)))

   ; calling
   (pass a)
   (pass a b)
   (pass a b c)
   (pass a b c d)
   (pass a (pass b c) d)
   (pass a b (pass c d))
   (pass a (pass b c d))
   ((car (pass pass)) c d)

   ; do
   (do () (#t))
   (do () (#t 4))
   (do ((i 1 (+ i 1))) ((> i 10)) (display i) (newline))
   (do ((i 1 2)(j 3 4)) (5 6) 7 8)

   ; delay
   (+ 1 2)
   (delay (+ 1 2))
   (delay-force (+ 5 9))

   ; cond-expand
   (cond-expand (tr7 #f) (else #t))
   (cond-expand (tr8 #f) (else #t))
   (cond-expand ((not tr8) #f) (else #t))
   (cond-expand ((and tr7 tr8) #f) (else #t))
   (cond-expand ((or tr8 tr7) #f) (else #t))
   (cond-expand (tr8 #f) (tr9 #f) (tr7 8) (else #t))

   ; guard
   (let () (guard (x (a => b) (x c) (else y)) u v w) z)
   (guard (x (a => b) (x c)) u v w)

   ; case-lambda
   (case-lambda ((a) a) ((a b) b) ((a b c) c) (x x))

   ; parameterize
   (let ((a (make-parameter 8))
         (b (make-parameter 9)))
         (cons (parameterize ((a 1)(b 2)) (cons (a) (b))) (cons (a) (b))))

   ; define-record-type
   (define-record-type <pare> (kons x y) pare? (x kar set-kar!) (y kdr))
   (define-record-type <pare> (kons x y) pare? (y kdr) (x kar set-kar!))
   (define-record-type <pare> kons       pare? (x kar set-kar!) (y kdr))
   (define-record-type <pare> #f         #f    (x kar set-kar!) (y kdr))
   (let ()
      (define-record-type <pare> (kons x y) pare? (x kar set-kar!) (y kdr))
      (define-record-type (<sub-pare> <pare>) sub-kons sub-pare? (sub sub set-sub!))
      <sub-pare>)

   ; syntax
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
))

(define (test expr)
   (newline)
   (write-shared expr)
   (display "\n  ==> ")
   (disass (compile expr))
   (call-with-values
      (lambda () (eval expr))
      (lambda result
         (if (null? result)
            (display "  no result!!\n")
            (for-each
               (lambda (item)
                  (display "  ... ")
                  (display item)
                  (newline))
               result))))
   (newline))

(for-each test test-cases)

; vim: noai ts=3 sw=3 expandtab
