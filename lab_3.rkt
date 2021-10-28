;#1
(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex object)
     (begin
       (display `object)
       (display " => ")
       (let ((result object))       
         (display result)
         (newline)
         result
         )))))
(define (zip . xss)
  (if (or (null? xss) (null? (trace-ex (car xss))))
      '()
      (cons (map car xss)
            (apply zip (map cdr (trace-ex xss))))))
(zip '(1 2 3) '(one two three))

(define counter
  (let ((x 0))
    (lambda ()
      (set! x (+ 1 x))
      x)))

(+ 1 (trace-ex (counter)))
(+ 1 (trace-ex (counter)))
;(trace-ex (/ 1 0))

;#2
(load "unit-test.scm")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0) 1) ;
    (else 1)))

(define the-tests
  (list (test (signum -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))

(run-tests the-tests)

;#3
(define (ref xs . args)
  
  (define (loop1 i xs1 arg)
    (if (null? xs1)
        #f
        (if (= i arg)
            (car xs1)
            (loop1 (+ i 1) (cdr xs1) arg)
            )
        )
    )

  (define (loop2 i xs1 pos elem)
    (if (null? xs1)
        '()
        (if (= pos i)
            (append (cons elem (list (car xs1))) (loop2 (+ i 1) (cdr xs1) pos elem))
            (cons (car xs1) (loop2 (+ i 1) (cdr xs1) pos elem))
            )
        )
    )
  
  (define s (cond ((vector? xs) (vector->list xs))
                  ((string? xs) (string->list xs))
                  (else xs)
                  )
   )
  
  (if (= (length args) 1)
      (loop1 0 s (car args))      
      (cond ((vector? xs) (list->vector (loop2 0 s (car args) (cadr args))))
            ((string? xs) (list->string (loop2 0 s (car args) (cadr args))))
            (else (loop2 0 s (car args) (cadr args)))                  
            )
      )
  )


(define the-tests
  (list (test (ref '(1 2 3) 1) 2)
        (test (ref #(1 2 3) 1) 2)
        (test (ref "123" 1) #\2)
        (test (ref "123" 3) #f)
        (test (ref '(1 2 3) 1 0) '(1 0 2 3))
        (test (ref #(1 2 3) 1 0) #(1 0 2 3))
        )
  )

(run-tests the-tests)

;#4




