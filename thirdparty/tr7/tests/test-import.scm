;
; vim: noai ts=3 sw=3 expandtab
(import (tr7 trace))
(tr7-show-eval #t)
(tr7-show-result #t)
(tr7-show-prompt #t)

(define-library (test-import)
   (import (scheme base) (prefix (srfi :1) s-))

   (include-library-declarations "test-import.scm.aux")

   (begin
      (define (hurray x) (list 'hurray x '!))))

(import (test-import))

; constructors

(s-xcons '(b c) 'a)

(s-cons* 1 2 3 4)

(s-circular-list 'z 'q)

(s-iota 5)

(s-hurray 'scheme)

(s-stomp 'big 'big 'little 'thing)

