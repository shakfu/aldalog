; sample of printing

(import (scheme write))

(define (print-shared item)

   (define (add-elem val res)
      (let ((p (assq val res)))
         (if p
            (begin
               (set-cdr! p (+ (cdr p) 1))
               res)
            (cons (cons val 0) res))))

   (define (list-elem val res)
      (if (pair? val)
         (let ((r (add-elem val res)))
            (if (eq? r res)
               res
               (list-elem (cdr val) (list-elem (car val) r))))
         res))

   (define (filter res)
      (cond
         ((null? res) res)
         ((zero? (cdar res)) (filter (cdr res)))
         (else (cons (cons (caar res) 0) (filter (cdr res))))))

   (define shared (filter (list-elem item ())))

   (define num 1)

   (define (prt val open)
      (if (not (pair? val))
         (display val)
         (let ((sh (assq val shared)))
            (if (and sh (not (zero? (cdr sh))))
               (begin
                  (display #\#)
                  (display (cdr sh))
                  (display #\#))
               (begin
                  (when sh
                     (set-cdr! sh num)
                     (set! num (+ num 1))
                     (display #\#)
                     (display (cdr sh))
                     (display #\=))
                  (if open
                     (display #\())
                  (let ((head (car val))
                        (next (cdr val)))
                     (prt head #t)
                     (if (and (pair? next) (not (assq next shared)))
                        (begin
                           (display " ")
                           (prt next #f))
                        (begin
                           (unless (null? next)
                              (display " . ")
                              (prt next #t))
                           (display #\))))))))))

   (prt item #t))

; vim: noai ts=3 sw=3 expandtab
