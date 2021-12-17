(load "unit-test.scm")
;(load "lab_3.rkt")

;#1

;<expression> := <content> FINISH-SYMBOL
;<content> := <fraction> | <other-symbols> | <content> <fraction> | <content> <other-symbols>
;<fraction> := <sign> <number> SLASH <number>
;<number> := <digit> | <number> <digit>
;<digit> := 0|1|2|3|4|5|6|7|8|9
;<sign> := - | +

;FINISH-SYMBOL
(define finish-symbol #\⛔)

(define (make-stream str)
  (append (string->list str) (list finish-symbol))
  )

(define (peek stream)
  (if (null? stream)
      #f
      (car stream)))

(define (next stream)
  (if (null? stream)
      #f
      (cdr stream))
  )

(define (next-col x stream)
  (letrec ((loop (lambda (xs x)
                   (if (or (= x 0) (null? xs))
                       xs
                       (loop (cdr xs) (- x 1))
                       )
                   ))
           )
    (loop stream x))
  )

;<sign> := - | +
(define (Sign? symb)
  (or (equal? symb #\-) (equal? symb #\+))
  )
(define (peak-sign char)
  (cond ((equal? char #\-) -1)
        (else 1))
  )
;<digit> := 0|1|2|3|4|5|6|7|8|9
(define (Digit? char)
  (or (equal? char #\0) (equal? char #\1) (equal? char #\2) (equal? char #\3) (equal? char #\4)
      (equal? char #\5) (equal? char #\6) (equal? char #\7) (equal? char #\8) (equal? char #\9))
  )
      
(define (scan-digit char)
  (cond ((equal? char #\0) 0) ((equal? char #\1) 1) ((equal? char #\2) 2) ((equal? char #\3) 3)
        ((equal? char #\4) 4) ((equal? char #\5) 5) ((equal? char #\6) 6) ((equal? char #\7) 7)
        ((equal? char #\8) 8) ((equal? char #\9) 9) (else #f))
  )
        
;<number> := <digit> | <number> <digit>
(define (Num? stream)
  (if (null? stream) 
      #f
      (letrec ((loop (lambda (xs)
                       (if (null? xs)
                           #t
                           (and (Digit? (peek xs)) (loop (cdr xs)))
                           )
                       ))
               )
        (loop stream)             
        )
      )
  )
(define (Num stream)
  (letrec ((loop (lambda (xs res)
                   (if (null? xs)
                       res
                       (loop (cdr xs) (+ (* res 10) (scan-digit (car xs))))
                       )
                   )))
    (loop stream 0)
    )
  )
    
;<fraction> := <sign> <number> SLASH <number>
(define (frac stream) 
  (letrec (
           (stream-numerator '())
           (stream-denominator '())
           (sign 1)
           (make-stream-loop (lambda (xs1 xs2 finish)
                               (if (or (equal? (peek xs1) finish) (equal? (peek xs1) finish-symbol) (null? xs1))
                                   xs2
                                   (make-stream-loop (cdr xs1) (append xs2 (list (peek xs1))) finish)
                                   )
                               ))
           )
    ;sign
    (if (Sign? (peek stream))
        (begin (set! sign (peak-sign (peek stream))) (set! stream (next stream)))
        )
    ;numerator
    (set! stream-numerator (make-stream-loop stream '() #\/))
    (set! stream (next-col (length stream-numerator) stream))

    ;SLASH
    (if (eq? (peek stream) #\/)
        (begin
          (set! stream (next stream))
          ;denominator
          (set! stream-denominator (make-stream-loop stream '() finish-symbol))
          (set! stream (next-col (length stream-denominator) stream))
          (if (and (Num? stream-numerator) (Num? stream-denominator))
              (list (* sign (Num stream-numerator)) (Num stream-denominator))
              #f
              )
          )
        #f)
    )
  )

;<content> := <fraction> | <other-symbols> | <content> <fraction> | <content> <other-symbols>
(define (fracs stream)
  (letrec ((x #f)
           (list-fracs '())
           (loop (lambda (xs fracs)
                   (if (equal? (peek xs) finish-symbol)
                       fracs
                       (if (or (Digit? (peek xs)) (Sign? (peek xs)))
                           (begin (set! x (try-frac (append xs (list finish-symbol))))
                                  (if (null? x)
                                      ;(loop (cdr xs) fracs)
                                      #f
                                      (loop (next-col (length x) xs) (append fracs (frac x)))
                                      ))
                           (if (null? xs)
                               fracs
                               (loop (cdr xs) fracs))
                       )
                   )))
           (try-frac (lambda (xs)
                       (letrec ((loop2 (lambda (xs1 xs2 res)
                                        (if (null? xs1)
                                            res
                                            (begin (set! xs2 (append xs2 (list (car xs1))))
                                                   (if (frac xs2)
                                                       (set! res xs2))
                                                   (loop2 (cdr xs1) xs2 res))
                                            )
                                        )))
                         (loop2 xs '() '())
                         )
                       ))
           (complete-fracs (lambda (xs1 xs2)
                             (if (null? xs1)
                                 xs2
                                 (complete-fracs (cdr (cdr xs1)) (append xs2 (list (/ (car xs1) (car (cdr xs1))))))
                                 )
                             ))
           )
    (set! list-fracs (loop stream '()))
    (if (eq? list-fracs #f)
        #f
        (complete-fracs list-fracs '())
        )
    )
  )

                       
                                     
                                     

(define (check-frac str)
  (if (frac (make-stream str))
      #t
      #f)
  )

(define (scan-frac str)
  (if (check-frac str)
      (/ (car (frac (make-stream str))) (car (cdr (frac (make-stream str)))))
      #f
      )
  )

(define (scan-many-fracs str)
  (fracs (make-stream str))
  )


(define tests (list
        (test (check-frac "110/111") #t)
        (test (check-frac "-4/3") #t)    
        (test (check-frac "+5/10") #t)   
        (test (check-frac "5.0/10") #f)
        (test (check-frac "FF/10") #f)
        (test (check-frac "/313") #f)
        (test (check-frac "-2a3/23") #f)

        (test (scan-frac "110/111") 110/111)
        (test (scan-frac "-4/3") -4/3)
        (test (scan-frac "+5/10") 1/2)
        (test (scan-frac "5.0/10") #f)
        (test (scan-frac "FF/10") #f)

        (test (scan-many-fracs "111/1233 11/222 -2/23") '(111/1233 11/222 -2/23))
        (test (scan-many-fracs "\t1/2 1/3\n\n10/8") '(1/2 1/3 5/4))
        (test (scan-many-fracs "\t1/2 1/3\n\n2/-5") #f)
        )
       )
(run-tests tests)

;#2
           
  
      
  



