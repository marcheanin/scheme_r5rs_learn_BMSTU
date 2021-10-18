(define (f xs)
  (if (null? xs)
      '()
      (if (list? xs)
          xs
          (if (pair? xs)
              xs
              (if (and (pair? (car xs)) (not (list? (car xs))))
                  (cons (f (car xs)) (f (cdr xs)))
                  (if (list? (car xs))
                      (append (car xs) (list (f (cdr xs))))
                      (cons (car xs) (list (f (cdr xs))))
                      )
                  )
              )
          )
      )
  )
(f '(a (b . c)))


