
(define-library (tr7 expr)

(import
   (scheme base))

(export
   expr)

(cond-expand
   (tr7-greedy-syntax

;--------------------------------
; when greedy syntax is available
(begin

#!no-greedy-syntax

(define-syntax expr
   (syntax-rules (not and or implies eqv)
      ((_ a b ... or      c . d) (or  (expr a b ...) (expr c . d)))
      ((_ a b ... and     c . d) (and (expr a b ...) (expr c . d)))
      ((_ a b ... implies c . d) (or (not (expr a b ...)) (expr c . d)))
      ((_ a b ... not eqv c . d) (not (eqv? (expr a b ...) (expr c . d))))
      ((_ a b ... eqv     c . d) (eqv? (expr a b ...) (expr c . d)))
      ((_ not a)                 (not (expr a)))
      ((_ a . b)                 (expr-r a . b))
   ))

#!greedy-syntax

(define-syntax expr-r
   (syntax-rules (< > = <= >= <>)
      ((_ a b ... <  c . d) (< (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... >  c . d) (> (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... =  c . d) (= (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... <= c . d) (<= (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... >= c . d) (>= (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... <> c . d) (not (= (expr-a a b ...) (expr-a c . d))))
      ((_ a . b)            (expr-a a . b))
   ))

(define-syntax expr-a
   (syntax-rules (+ - * / // % ^)
      ((_ ((a b ...)))      (a b ...))
      ((_ (a b ...))        (expr-a a b ...))
      ((_ a b ... + c . d)  (+ (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... - c . d)  (- (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... * c . d)  (* (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... / c . d)  (/ (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... // c . d) (quotient (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... % c . d)  (remainder (expr-a a b ...) (expr-a c . d)))
      ((_ a b ... ^ c . d)  (expt (expr-a a b ...) (expr-a c . d)))
      ((_ - a)              (- (expr-a a)))
      ((_ a)                a)
      ((_ . a)              (syntax-error "bad expr" a))
   ))

))
(else

;------------------------------------
; when greedy syntax is not available
(begin
  (error "expr is not not implemented yet")
))

))

