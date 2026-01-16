(define-syntax p
  (syntax-rules ()
    ((_ ?e ...) (begin (for-each display (list (quote ?e) " -> " ?e "\n")) ...))))

(p
(+ 1 5)
(+ 1 (square 5))
(+ 1 5 (square 5))
)
