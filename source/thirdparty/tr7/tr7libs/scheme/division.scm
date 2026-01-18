;; implementation of SRFI-141

(import (scheme base))

;; euclidean/ family

(define (euclidean/ numerator denominator)
   (let-values (((q r) (floor/ numerator denominator)))
      (if (or (positive? denominator) (zero? r))
         (values q r)
         (values (+ q 1) (- r denominator)))))

(define (euclidean-quotient numerator denominator)
   (let-values (((q r) (euclidean/ numerator denominator)))
      q))

(define (euclidean-remainder numerator denominator)
   (let-values (((q r) (euclidean/ numerator denominator)))
      r))

;; formating the result

(define (-quotient quotient remainder)
   quotient)

(define (-remainder quotient remainder)
   remainder)

(define (-values quotient remainder)
   (values quotient remainder))

;; computing adaptation

(define (/ceiling q r denominator)
   (not (or (zero? r) (negative? denominator))))

(define (/round q r denominator)
   (let ((e (- (+ r r) (abs denominator))))
     (or (positive? e)
         (and (zero? e) (odd? q)))))

(define (/balanced q r denominator)
   (>= (+ r r) (abs denominator)))

;; compute the quotient and compose the result

(define (divide numerator denominator adapt return)
   (let*-values (((q0 r0)  (euclidean/ numerator denominator))
                 ((adapt?) (adapt q0 r0 denominator)))
      (if (not adapt?)
         (return q0 r0)
         (if (negative? denominator)
            (return (- q0 1) (+ r0 denominator))
            (return (+ q0 1) (- r0 denominator))))))

;; ceiling/ family

(define (ceiling/ numerator denominator)
   (divide numerator denominator /ceiling -values))

(define (ceiling-quotient numerator denominator)
   (divide numerator denominator /ceiling -quotient))

(define (ceiling-remainder numerator denominator)
   (divide numerator denominator /ceiling -remainder))

;; round/ family

(define (round/ numerator denominator)
   (divide numerator denominator /round -values))

(define (round-quotient numerator denominator)
   (divide numerator denominator /round -quotient))

(define (round-remainder numerator denominator)
   (divide numerator denominator /round -remainder))

;; balanced/ family

(define (balanced/ numerator denominator)
   (divide numerator denominator /balanced -values))

(define (balanced-quotient numerator denominator)
   (divide numerator denominator /balanced -quotient))

(define (balanced-remainder numerator denominator)
   (divide numerator denominator /balanced -remainder))

