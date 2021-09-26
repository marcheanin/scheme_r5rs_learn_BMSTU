(define pi (* 4 (atan 1)))
pi

(define (hypot a b)
  (sqrt(+ (* a a) (* b b))))
(hypot 3 4)

(if #f 2 1)
(if (> 3 2) 1 0)
(* 100 (if #t 2 4))

(or #f #f 2)
(and 2 3 #f)

(not 5)

(define (factorial n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 5)

(define (factorial2 n)
  (define (loop i acc)
    (if (> i n)
        acc
        (loop (+ i 1) (* i acc))))
  (loop 1 1))

(if (and (< 5 3) (< 2 3))
    #t
    #f)
(define a 1)
(- a 2)
a
  
