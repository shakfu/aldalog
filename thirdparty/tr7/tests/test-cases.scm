;;; testing of tr7
(import (tr7 trace) (tr7 gc) (scheme char))

(tr7-gc-verbose #t)
(tr7-gc)
;(tr7-tracing 1)

(tr7-show-eval #t)
(tr7-show-result #t)
(tr7-show-prompt #t)

(tr7-gc)
#\alarm #\backspace #\delete #\escape
#\newline #\null #\return #\space #\tab
#\x07 #\x08 #\x7f #\x1b #\x0a #\x00 #\x0d #\x20 #\x09
#\a #\  #\( #\) #\" #\'

(tr7-gc)
(+ 1 2 3 4 5)
(* 1 2 3 4 5)
(* 3.14 5)

(tr7-gc)
(memq 'a '(a b c))
(memq 'b '(a b c))
(memq 'a '(b c d))
(memq (list 'a) '(b (a) c))
(member (list 'a) '(b (a) c))
(member "B" '("a" "b" "c") string-ci=?)
(memq 101 '(100 101 102))
(memv 101 '(100 101 102))

(tr7-gc)
(if #t 1)
(if #f 1)
(if #f 1 2)
(if #t 1 2)

(tr7-gc)
;;; at end
