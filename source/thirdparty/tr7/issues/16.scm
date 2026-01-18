

(cond-expand

  ((library (srfi 69))

     (import (srfi 69))

     (define (db-new)
          (make-hash-table))

  )(else

     (define (db-new)
          (cons #f '()))

  ))

(define *prolog-database* (make-parameter (db-new)))

(define (new-database!)
  (*prolog-database* (db-new)))

(new-database!)

