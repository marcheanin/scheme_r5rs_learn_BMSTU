;#1
(define-syntax trace-ex
  (syntax-rules ()
    ((trace-ex object)
     (let ((result object))
       (begin
         (display `object)
         (display " => ")
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

