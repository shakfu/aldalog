
(define-syntax dodo
  (syntax-rules (xxx)
    ((dodo a ... xxx b ...) (display (list (list b ...) (list a ...))))
    ((dodo a ... ) (display (list (list ) (list a ...))))
  ))

(dodo 1 2 3)
(newline)
(dodo 1 2 3 xxx 4 5 6)
(newline)

