(define (make-multi-vector . xs)
  (define sizes (car xs))
  (define fill (if (null? (cdr xs))
                   0
                   (car (cdr xs))
                   )
    )
  xs
  )
(define m (make-multi-vector '(11 12 9 16) 5))
m
(define (make-list xs)
  (if (null? xs)
      '()
      (cons 0 (make-list (cdr xs)))
      )
  )
(define xs (list 2 2 3))
(define cord (make-list xs))
cord
(equal? (list 1 2 3 4) (list 1 2 3 4))

(define (next-cord cord xs)
  (if (null? cord)
      '()
      (if (< (car cord) (- (car xs) 1))
          (cons (+ (car cord) 1) (cdr cord))
          (cons 0 (next-cord (cdr cord) (cdr xs)))
          )
      )
  )
(define cord (list 0 0 0))
(define (loop i n)
  (if (< i n)
      (begin (set! cord (next-cord cord xs)) (display cord) (loop (+ i 1) n))
      '())
  )
  
(loop 0 10)


(define (pr xs)
  (if (null? xs)
      1
      (* (car xs) (pr (cdr xs)))
      )
  )
(pr xs)
(define sizes (list 1 22 3))
sizes








  