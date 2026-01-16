(define-syntax dodo
  (syntax-rules ()
    ((dodo ((var init step ...) ...)
          (test expr ...)
          command ...)
     (letrec
       ((loop
           (lambda (var ...)
               (if test
                    (begin
                         (if #f #f)
                         expr ...)
                    (begin
                         command
                         ...
                         (loop (dodo "step" var step ...) ...))))))
        (loop init ...)))
     ((dodo "step" x)
        x)
     ((dodo "step" x y)
        y)
))

(dodo ((i 0 (+ i 1)) (n2 40 (- n2 1)))
      ((> i n2) i)
      (display i)
      (display "\n"))

