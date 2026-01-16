(import (scheme division) (scheme write) (scheme cxr))

(define (resu n d q)
   (let* ((q (exact q))
          (r (- n (* q d))))
      (values q r)))

(define (div n d norm)
   (resu n d (norm (/ n d))))

(define (div-ceiling   n d)  (div n d ceiling))
(define (div-floor     n d)  (div n d floor))
(define (div-truncate  n d)  (div n d truncate))
(define (div-round     n d)  (div n d round))
(define (div-euclidean n d)  (div n d (if (negative? d) ceiling floor)))
(define (div-balanced  n d)
   (let-values (((ad inc) (if (negative? d)
                                (values (- d) -1)
                                (values d 1))))
      (let loop ((q 0))
         (let ((r (- n (* q d))))
            (if (negative? r)
               (if (negative? (+ r r ad))
                  (loop (- q inc))
                  (values q r))
               (if (negative? (- (+ r r) ad))
                  (values q r)
                  (loop (+ q inc))))))))
         
(define ops `(
   (ceiling/    ,ceiling/   ,div-ceiling)
   (floor/      ,floor/     ,div-floor)
   (truncate/   ,truncate/  ,div-truncate)
   (round/      ,round/     ,div-round)
   (euclidean/  ,euclidean/ ,div-euclidean)
   (balanced/   ,balanced/  ,div-balanced)
))

(define vals '(
   (14 . 4)
   (15 . 4)
   (16 . 4)
   (17 . 4)
   (18 . 4)
))

(define (prt p n d)
   (let-values (((q r) ((cadr p) n d))
                ((Q R) ((caddr p) n d)))
      (for-each display (list "(" (car p) " " n " " d ")\t\t" q " " r "\t"))
      (if (and (= q Q) (= r R))
         (display "OK\n")
         (for-each display (list "ERR: " Q " " R "\n")))))

(define (tst p v)
   (let ((n (car v))
         (d (cdr v)))
     (prt p n     d)
     (prt p (- n) d)
     (prt p n     (- d))
     (prt p (- n) (- d))))

(import (scheme list))
(define vals (apply append (map (lambda (d) (map (lambda (n) (cons n d)) (iota (+ d d 1) d))) (iota 9 1))))
   
(for-each (lambda (p) (for-each (lambda (v) (tst p v)) vals)) ops)


