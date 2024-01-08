(define foo 21)
(define x 5)
(define value (* x foo))
p
(* value 4)
(define value (* 4 (+ 1 2)))
value

(define value (* (define x 5) (define foo 21)))
(define value (* 5 21))