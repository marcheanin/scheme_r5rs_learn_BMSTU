;#3
(define and-fold (lambda x 
                   (if (= (length x) 1)
                       (car x)
                       (and (car x) (apply and-fold (cdr x)))
                       )
                   )
  )

;#4
(define (f xs)
  (cond ((not (pair? xs)) xs)
        ((and (list? xs) (null? xs)) xs)
        ((list? xs) (cons (f (car xs)) (f (cdr xs))))
        ((pair? (cdr xs)) (cons (f (car xs)) (f (cdr xs))))
        (else (list (f (car xs)) (cdr xs)))))

