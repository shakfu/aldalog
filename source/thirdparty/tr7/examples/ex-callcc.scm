(define a #f)
(define b #f)
(define n 10)

(call/cc (lambda (cont)
	(call/cc (lambda (mycont) (set! b mycont) (cont 1)))
	(unless (zero? n)
		(set! n (- n 1))
		(display "HELLO")
		(newline)
		(a))
        'first))

(unless a
	(call/cc (lambda (cont)
		(call/cc (lambda (mycont) (set! a mycont) (cont 2 3)))
		(unless (zero? n)
			(set! n (- n 1))
			(display "BYE")
			(newline)
			(b))
		'second)))

(unless (zero? n) (a))


(let ((path '())
      (c #f))
   (let ((add (lambda (s)
                 (set! path (cons s path)))))
            (add (call-with-current-continuation
               (lambda (c0)
                  (set! c c0)
                  'talk1))))
   (if (< (length path) 2)
       (c 'talk2)
))

(let ((i #f))
	(display (call-with-current-continuation (lambda (c) (set! i c) "first")))
	(newline)
	(when i (let ((proc i)) (set! i #f) (proc "second"))))


(define i #f)(display (call/cc (lambda (c) (set! i c) "toto")))(newline)

(define i #f)
(define a 1)
(define b 2)
(display (list a (call/cc (lambda (c) (set! i c) 0)) b))
(newline)
(when i
   (let ((c i))
     (set! i #f)
     (set! a 4)
     (set! b 5)
     (c 9)))

