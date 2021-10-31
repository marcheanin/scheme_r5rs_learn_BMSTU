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
  (define (loop tests flag)
    (if (null? tests)
        (begin (display #\newline) flag)
        (if (run-test (car tests))
            (loop (cdr tests) flag)
            (loop (cdr tests) #f)
            )
        )
    )
  (loop the-tests #t)
  )