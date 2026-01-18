
(define (test x)
   (let ((vec #(#f #f #f #f))
         (val x))
      (unless (vector-ref vec 0)
         (call/cc
            (lambda (cont)
               (dynamic-wind
                  (lambda () (display "[0:"))
                  (lambda ()
                     (unless (vector-ref vec 0)
                        (call/cc
                           (lambda (proc) 
                              (vector-set! vec 0 proc)
                              (cont #f))))
                     (display "-0-"))
                  (lambda () (display ":0]"))))))
      (display "<0>")
      (unless (vector-ref vec 1)
         (call/cc
            (lambda (cont)
               (dynamic-wind
                  (lambda () (display "[1:"))
                  (lambda ()
                     (unless (vector-ref vec 1)
                        (call/cc
                           (lambda (proc) 
                              (vector-set! vec 1 proc)
                              (cont #f))))
                     (display "-1-"))
                  (lambda () (display ":1]"))))))
      (display "<1>")
      (unless (vector-ref vec 2)
         (call/cc
            (lambda (cont)
               (dynamic-wind
                  (lambda () (display "[2:"))
                  (lambda ()
                     (unless (vector-ref vec 2)
                        (call/cc
                           (lambda (cont)
                              (dynamic-wind
                                 (lambda () (display "[2.1:"))
                                 (lambda ()
                                    (unless (vector-ref vec 2)
                                       (call/cc
                                          (lambda (proc) 
                                             (vector-set! vec 2 proc)
                                             (cont #f))))
                                    (display "-2.1-"))
                                 (lambda () (display ":2.1]"))))))
                     (display "<2.1>")
                     (unless (vector-ref vec 3)
                        (call/cc
                           (lambda (cont)
                              (dynamic-wind
                                 (lambda () (display "[2.2:"))
                                 (lambda ()
                                    (unless (vector-ref vec 3)
                                       (call/cc
                                          (lambda (proc) 
                                             (vector-set! vec 3 proc)
                                             (cont #f))))
                                    (display "-2.2-"))
                                 (lambda () (display ":2.2]"))))))
                     (display "<2.2>"))
                  (lambda () (display ":2]"))))))
      (display "<2>")
      (newline)
      (unless (zero? val)
         (let ((q (quotient val 4))
               (r (remainder val 4)))
            (set! val q)
            ((vector-ref vec r) #f))))
      (newline))

(test #b11100100)

;[0::0]<0>[1::1]<1>[2:[2.1::2.1]<2.1>[2.2::2.2]<2.2>:2]<2>
;[0:-0-:0]<0><1><2>
;[1:-1-:1]<1><2>
;[2:[2.1:-2.1-:2.1]<2.1><2.2>:2]<2>
;[2:[2.2:-2.2-:2.2]<2.2>:2]<2>


