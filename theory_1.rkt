(define (my-flatten xs)
  (define (loop xs my_xs)
    (if (list? (car xs))
        (loop (cdr xs) (append xs my_xs))
        (loop (cdr xs) (append xs (list my_xs)))))
  (loop xs '()))

(my-flatten '((1) 2 (3 (4 5)) 6))