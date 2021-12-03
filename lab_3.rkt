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
(define counter-tests
  (list
    (test (counter) 1)   ; OK
    (test (counter) 3)   ; FAIL
    (test (counter) 3))) ; OK
(run-tests counter-tests)
(+ 1 (trace-ex (counter)))
(+ 1 (trace-ex (counter)))
;(trace-ex (/ 1 0))

;#2
(load "unit-test.scm")

(define (signum x)
  (cond
    ((< x 0) -1)
    ((= x 0) 1) 
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
        (if (= pos i)
            (list elem)
            '())
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
      (cond ((vector? xs) (if (and (<= 0 (car args)) (<= (car args) (vector-length xs)))
                              (list->vector (loop2 0 s (car args) (cadr args)))
                              #f))
            ((string? xs) (if (and (<= 0 (car args)) (<= (car args) (string-length xs)) (char? (cadr args)))
                              (list->string (loop2 0 s (car args) (cadr args)))
                              #f))
            (else (if (and (<= 0 (car args)) (<= (car args) (length xs)))
                      (loop2 0 s (car args) (cadr args))
                      #f))
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
        (test (ref "123" 1 #\0) "1023")
        (test (ref "123" 1 0) #f)
        (test (ref "123" 3 #\4) "1234")
        (test (ref "123" 5 #\4) #f)
        (test (ref "123" -2 #\4) #f)
        )
  )

(run-tests the-tests)

;#4

(define (factorize ex)
  (define sign (car ex))
  (define x (ref (ref ex 1) 1))
  (define y (ref (ref ex 2) 1))
  (define pow (ref (ref ex 1) 2))
  (if (eq? pow 2)
      (if (eq? sign '-)
          `(* (- ,x ,y) (+ ,x ,y))
          #f
          )
      (if (eq? sign '-)
          `(* (- ,x ,y) (+ (+ (expt ,x 2) (expt ,y 2)) (* ,x ,y)))
          `(* (+ ,x ,y) (- (+ (expt ,x 2) (expt ,y 2)) (* ,x ,y)))
          )
      )
  )


(define the-tests
  (list (test (factorize '(- (expt x 2) (expt y 2))) '(* (- x y) (+ x y)))
        (test (factorize '(- (expt (+ first 1) 2) (expt (- second 1) 2))) '(* (- (+ first 1) (- second 1))
                                                                              (+ (+ first 1) (- second 1))))
        (test (eval (list (list 'lambda 
                                '(x y) 
                                (factorize '(- (expt x 2) (expt y 2))))
                          1 2)
                    (interaction-environment)) -3)
        (test (factorize '(- (expt a 3) (expt b 3))) '(* (- a b) (+ (+ (expt a 2) (expt b 2)) (* a b))))
        (test (factorize '(+ (expt a 3) (expt b 3))) '(* (+ a b) (- (+ (expt a 2) (expt b 2)) (* a b))))
        )
  )

(run-tests the-tests)



