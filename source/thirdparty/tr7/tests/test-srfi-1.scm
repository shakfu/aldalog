;
; vim: noai ts=3 sw=3 expandtab
(import (tr7 trace))
(tr7-show-eval #t)
(tr7-show-result #t)
(tr7-show-prompt #t)

(import (srfi :1))

; constructors

(xcons '(b c) 'a)

(cons* 1 2 3 4)
(cons* 1)

(list-tabulate 4 values)
(list-tabulate 4 (lambda (i) i))

(circular-list 'z 'q)

(iota 5)
(iota 5 0 -0.1)

; predicates

; selectors

(third '(a b c d e))

(take '(a b c d e)  2)
(drop '(a b c d e)  2)

(take '(1 2 3 . d) 2)
(drop '(1 2 3 . d) 2)
(take '(1 2 3 . d) 3)
(drop '(1 2 3 . d) 3)

(take-right '(a b c d e) 2)
(drop-right '(a b c d e) 2)

(take-right '(1 2 3 . d) 2)
(drop-right '(1 2 3 . d) 2)
(take-right '(1 2 3 . d) 0)
(drop-right '(1 2 3 . d) 0)

(take! (circular-list 1 3 5) 8)

(split-at '(a b c d e f g h) 3)

(last '(a b c))
(last-pair '(a b c))

; miscellanous

(reverse '(a b c))
(reverse '(a (b c) d (e (f))))

(zip '(one two three)
     '(1 2 3)
     '(odd even odd even odd even odd even))
(zip '(1 2 3))
(zip '(3 1 4 1) (circular-list #f #t))

(unzip2 '((1 one) (2 two) (3 three)))

(count even? '(3 1 4 1 5 9 2 5 6))
(count < '(1 2 4 8) '(2 4 6 8 10 12 14 16))
(count < '(3 1 4 1) (circular-list 1 10))

; fold, unfold, map

(fold cons* '() '(a b c) '(1 2 3 4 5))
(fold-right cons* '() '(a b c) '(1 2 3 4 5))

(unfold (lambda (x) (> x 10))
        (lambda (x) (* x x))
	(lambda (x) (+ x 1))
	1)

(unfold-right zero?
              (lambda (x) (* x x))
              (lambda (x) (- x 1))
              10)

(append-map! (lambda (x) (list x (- x))) '(1 3 8))

(pair-for-each (lambda (pair) (display pair) (newline)) '(a b c))

(filter-map (lambda (x) (and (number? x) (* x x))) '(a 1 b 3 c 7))

; filtering & partitionning

(filter even? '(0 7 8 8 43 -4))

(partition symbol? '(one 2 3 four five 6))

(remove even? '(0 7 8 8 43 -4))

; searching

(find even? '(3 1 4 1 5 9))

(find-tail even? '(3 1 37 -8 -5 0 0))
(find-tail even? '(3 1 37 -5))

(take-while even? '(2 18 3 10 22 9))
(drop-while even? '(2 18 3 10 22 9))

(span even? '(2 18 3 10 22 9))
(break even? '(3 1 4 1 5 9))

(any integer? '(a 3 b 2.7))
(any integer? '(a 3.1 b 2.7))
(any < '(3 1 4 1 5) '(2 7 1 8 2))

(list-index even? '(3 1 4 1 5 9))
(list-index < '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))
(list-index = '(3 1 4 1 5 9 2 5 6) '(2 7 1 8 2))

; deletion

(delete-duplicates '(a b a c a b c z))

(delete-duplicates '((a . 3) (b . 7) (a . 9) (c . 1))
                   (lambda (x y) (eq? (car x) (car y))))

; association lists

; set operations on lists

(lset<= eq? '(a) '(a b a) '(a b c c))
(lset<= eq?)
(lset<= eq? '(a))

(lset= eq? '(b e a) '(a e b) '(e e b a))

(lset-adjoin eq? '(a b c d c e) 'a 'e 'i 'o 'u)

(lset-union eq? '(a b c d e) '(a e i o u))
(lset-union eq? '(a a c) '(x a x))
(lset-union eq?)
(lset-union eq? '(a b c))

(lset-intersection eq? '(a b c d e) '(a e i o u))
(lset-intersection eq? '(a x y a) '(x a x z))
(lset-intersection eq? '(a b c))

(lset-difference eq? '(a b c d e) '(a e i o u))
(lset-difference eq? '(a b c))

(lset-xor eq? '(a b c d e) '(a e i o u))
(lset-xor eq?)
(lset-xor eq? '(a b c d e))

