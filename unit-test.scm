(define-syntax test
  (syntax-rules (list)
    ( (test def res )
      (begin
        (let ((xs (quote def))
              (result res))
          (list xs result))))
    ;( (test def (list ress))
    ;  (begin
    ;   (let ((xs (quote def)) (results ress))
    ;     (list xs results))))
    ))

(define (run-test xs)
  (display #\newline)
  (display (car xs))
  (let
      ((head (eval (car xs)     
                   (interaction-environment)))
       (expect (cadr xs)))
    ;(newline)
    ;(display head)
    ;(newline)
    ;(display expect)
    
    (define (check expects)
      (if (null? expects)
          #f
          (or (equal? (car expects) head) (check (cdr expects)))
          )
      )
      
    
    (if (or (equal? head expect) (and (list? expect) (check expect)))
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
        (begin (newline) flag)
        (if (run-test (car tests))
            (loop (cdr tests) flag)
            (loop (cdr tests) #f)
            )
        )
    )
  (loop the-tests #t)
  )