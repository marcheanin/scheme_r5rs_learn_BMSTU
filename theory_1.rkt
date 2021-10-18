(define (and-fold xs)
  (if (null? xs)
      #t
      (and (car xs) (and-fold (cdr xs)))
      )
  )
(and-fold '(1 #f 2))

(define and-l (lambda x 
                (if (null? x)
                    #t
                    (if (car x) (apply and-l (cdr x)) #f))))

(define and-fold (lambda x 
                   (if (null? x)
                       #t
                       (and (car x) (apply and-fold (cdr x)))
                       )
                   )
  )
(define and-fold2 (lambda x 
                    (if (= (length x) 1)
                        (car x)
                        (and (car x) (apply and-fold2 (cdr x)))
                        )
                    )
  )


(apply and-fold '(5 #t #t))
(apply and-fold '(1 #f 2))

(apply and-fold2 '(#t 5 #t))
(and #t 5 #t)

(define (f xs)
  (if (null? (cdr xs))
      (car xs)
      (if (pair? (car xs))
          (cons (list (f (car xs))) (f (cdr xs)))
          (if (or (not (pair? (cdr xs))) (and (list? (cdr xs)) (not (list? xs))))
              (list (append (list (car xs)) (append (list (cdr xs)) '())))
              (append (list (car xs)) (f (cdr xs)))))))

(f '(a b . c))






