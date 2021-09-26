(define (my-odd? a)
  (= (remainder a 2) 0))
(my-odd? 2)

(define (my-even? a)
  (= (remainder a 2) 1))
(my-even? 3)