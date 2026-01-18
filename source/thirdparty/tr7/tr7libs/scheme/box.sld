; define here the library in case USE_SCHEME_BOX is 0
(define-library (scheme box)
  (export box box? unbox set-box!)
  (begin
     (define-record-type <box>
        (box value)
        box?
        (value unbox set-box!))))
