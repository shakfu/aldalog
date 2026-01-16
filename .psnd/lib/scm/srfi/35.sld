(define-library (srfi 35)
  (export
    make-condition-type
    condition-type?
    make-condition
    condition?
    condition-has-type?
    condition-ref
    make-compound-condition
    extract-condition
    define-condition-type
    condition
    &condition
    &message
    message-condition?
    &serious
    serious-condition?
    &error
    error?)
  (include "35.scm")
)

