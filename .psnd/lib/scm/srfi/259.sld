(define-library (srfi 259)
  (import (scheme base) (tr7 tagged-closures))
  (export define-procedure-tag)
  (begin
    (define (create-new-closure x)
      (cond
        ((closure? x) (closure-copy x))
        ((procedure? x) (closure-copy (lambda args (apply x args))))
        (else (error "not a procedure" x))))
    (define (tag-closure x tag value)
      (closure-set-tag!
       x
       (let loop ((tags (closure-get-tag x)))
         (cond
           ((null? tags) (list (cons tag value)))
           ((not (eq? (caar tags) tag))
            (cons (car tags) (loop (cdr tags))))
           (else (cons (cons tag value) (cdr tags)))))))
    (define genid
      (let ((x 0))
        (lambda ()
          (set! x (+ x 1))
          (list x))))
    (define (tag-pair x tag)
      (assq tag (closure-get-tag x)))
    (define-syntax define-procedure-tag
      (syntax-rules ()
        ((_ cstr pred? accessor)
         (define-values (cstr pred? accessor)
           (let ((tag (genid)))
             (values (lambda (value proc)
                       (let ((closure (create-new-closure proc)))
                         (tag-closure closure tag value)
                         closure))
                     (lambda (x)
                       (and (closure? x)
                            (tag-pair x tag)
			    #true))
                     (lambda (closure)
		       (cond
                         ((tag-pair closure tag) => cdr)
                         (else (error "not tagged with this" closure))))))))))))
