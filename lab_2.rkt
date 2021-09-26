
;#1
(define (count x xs)
  (if (null? xs)
      0    
      (if (equal? x (car xs))
          (+ 1 (count x (cdr xs)))
          (count x (cdr xs)) 
          )
      )
  )
(count 'a '(a b c a))

;#2
(define (delete pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (delete pred? (cdr xs))
          (cons (car xs) (delete pred? (cdr xs))))
      )
  )
(delete even? '(1 2 3 4))
(delete odd? '(2 4 6 8))
(delete even? '(2 4 6 8))



;#3
(define (iterate f x y)
  (if (= y 0)
      '()
      (cons x (iterate f (f x) (- y 1)))
      )
  )
  
(iterate (lambda (x) (* 2 x)) 1 1)
(iterate (lambda (x) (* 2 x)) 1 6)
(iterate (lambda (x) (* 2 x)) 1 0)
  
;#4
(define (intersperse x xs)
  (if (null? xs)
      '()
      (if (null? (cdr xs))
          (cons (car xs) (intersperse x (cdr xs)))
          (cons (car xs) (cons x (intersperse x (cdr xs))))
          )
      )
  )

(intersperse '+ '(1 2 3 4))
(intersperse 'x '(1 2))
(intersperse 'x '(1))
(intersperse 'x '())

;#5
(define (all? pred? xs)
  (if (null? xs)
      #t
      (and (pred? (car xs)) (all? pred? (cdr xs)))
      )
  )
(all? odd? '(1 3 5 7))
(all? odd? '())

(define (any? pred? xs)
  (if (null? xs)
      #f
      (or (pred? (car xs)) (all? pred? (cdr xs)))
      )
  )
(any? odd? '(1 3 5 7))
(any? odd? '(0 2 4 8))
(any? odd? '())

;#6

(define (f x) (+ x 2))
(define (g x) (* x 3))
(define (h x) (- x))

(define (o . xs)
  (lambda (x)
    (define (oo xs x)
      (if (null? xs)
          x
          (oo (cdr xs) ((car xs) x))
          )
      )
    
    (oo (reverse xs) x)
    )
  )

((o f g h) 1)
((o f g) 1)
((o h) 1)
((o) 1)