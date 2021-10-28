(define-syntax test
  (syntax-rules ()
    ( (test def res )
      (begin
        (let ((xs (quote def))
              (result res))
          (list xs result))))))

(define (run-test xs)
  (display #\newline)
  (display (car xs))
  (let
      ((head (eval (car xs)
                   (interaction-environment)))
       (expect (cadr xs)))

    (if (equal? head expect)
        (begin
          (display " ok")
          #t)
        (begin
          (display " FAIL")
          (display #\newline)
          (display "  Expected:")
          (display expect)
          (display #\newline)
          (display "  Returned:")
          (display head)
          #f))))

(define (run-tests the-tests)
  (if (null? the-tests)
      (begin (display #\newline) #t)
      (and (run-test (car the-tests)) (run-tests (cdr the-tests)))
      )
  )