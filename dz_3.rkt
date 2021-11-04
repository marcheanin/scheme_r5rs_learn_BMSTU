(load "unit-test.scm")

(define (derivative xs)
  
  (cond ((= (length xs) 1) (if (number? (car xs))
                               '(0)
                               '(1)))
        ((= (length xs) 2) (cond ((eq? (car xs) 'sin) (if (list? (list-ref xs 1))
                                                          `(* ,(derivative (list-ref xs 1)) (cos ,(list-ref xs 1))) 
                                                          `(cos ,(list-ref xs 1))))
                                 ((eq? (car xs) 'cos) (if (list? (list-ref xs 1))
                                                          `(* ,(derivative (list-ref xs 1)) (- (sin ,(list-ref xs 1)))) 
                                                          `(- (sin ,(list-ref xs 1)))))
                                 ((eq? (car xs) 'ln) (if (list? (list-ref xs 1))
                                                         `(* ,(derivative (list-ref xs 1)) (/ 1 ,(list-ref xs 1)))
                                                         `(/ 1 ,(list-ref xs 1)))
                                                     )
                                 (else (cons (car xs) (derivative (cdr xs))))))
        ((= (length xs) 3) (cond ((eq? (car xs) '*) (cond ((and (list? (list-ref xs 2)) (list? (list-ref xs 1))) `(+ (* ,(derivative (list-ref xs 1)) ,(list-ref xs 2)) (* ,(list-ref xs 1) ,(derivative (list-ref xs 2)))))
                                                          ((list? (list-ref xs 2)) `(* ,(list-ref xs 1) ,(derivative (list-ref xs 2))))
                                                          ((number? (list-ref xs 1)) `(* ,(list-ref xs 1) ,@(derivative (list (list-ref xs 2)))))
                                                          (else (derivative `(expt ,(list-ref xs 1) 2))))
                                                    )
                                 ((eq? (car xs) '+) (if (list? (list-ref xs 1))
                                                        `(+ ,(derivative (list-ref xs 1)) ,(derivative (list-ref xs 2)))
                                                        `(+ ,(derivative (list (list-ref xs 1))) ,@(derivative (list (list-ref xs 2))))))
                                 ((eq? (car xs) '-) (if (list? (list-ref xs 2))
                                                        `(- ,(derivative (list-ref xs 1)) ,(derivative (list-ref xs 2)))
                                                        `(- ,(derivative (list-ref xs 1)) ,@(derivative (list (list-ref xs 2))))))
                                 ((eq? (car xs) 'expt) (cond ((and (number? (list-ref xs 1)) (not (list? (list-ref xs 2)))) `(* (expt ,(list-ref xs 1) ,(list-ref xs 2)) (ln ,(list-ref xs 1))))
                                                             ((and (number? (list-ref xs 2)) (not (eq? (list-ref xs 1) 'e))) `(* ,(list-ref xs 2) (expt ,(list-ref xs 1) ,(- (list-ref xs 2) 1))))
                                                             ((and (list? (list-ref xs 2)) (eq? (list-ref xs 1) 'e)) `(* ,(derivative (list-ref xs 2)) (expt e ,(list-ref xs 2))))
                                                             ((and (list? (list-ref xs 2)) (not (eq? (list-ref xs 1) 'e))) `(* ,(derivative (list-ref xs 2)) (expt ,(list-ref xs 1) ,(list-ref xs 2)) (ln ,(list-ref xs 1))))
                                                             (else `(expt e ,(list-ref xs 2))))
                                                       )
                                 ((eq? (car xs) '/)  `(/ (- (* ,@(derivative (list (list-ref xs 1))) ,(list-ref xs 2)) (* ,(list-ref xs 1) ,@(derivative (list (list-ref xs 2))))) (expt ,(list-ref xs 2) 2)))
                                 )
                           )
                           
        )
  )


(define the-tests
  (list (test (derivative '(a)) '(1))
        (test (derivative '(5))  '(0))
        (test (derivative '(- x))  '(- 1))
        (test (derivative '(- 5))  '(- 0))
        (test (derivative '(* -1 x))  '(* -1 1))
        (test (derivative '(* -4 x))  '(* -4 1))
        (test (derivative '(- (* 2 x) 3))  '(- (* 2 1) 0))
        (test (derivative '(- (* 2 a) a))  '(- (* 2 1) 1))
        (test (derivative '(expt 5 x))  '(* (expt 5 x) (ln 5)))
        (test (derivative '(expt x 10))  '(* 10 (expt x 9)))
        (test (derivative '(* 2 (expt a 5)))  '(* 2 (* 5 (expt a 4))))
        (test (derivative '(- (* 2 x) 3))  '(- (* 2 1) 0))
        (test (derivative '(expt x -2)) '(* -2 (expt x -3)))
        (test (derivative '(cos x)) '(- (sin x)))
        (test (derivative '(sin x)) '(cos x))
        (test (derivative '(expt e x)) '(expt e x))
        (test (derivative '(* 2 (expt e x))) '(* 2 (expt e x)))
        (test (derivative '(* 2 (expt e (* 2 x)))) '(* 2 (* (* 2 1) (expt e (* 2 x)))))
        (test (derivative '(ln x)) '(/ 1 x))
        (test (derivative '(* 3 (ln x))) '(* 3 (/ 1 x)))
        (test (derivative '(+ (expt x 3) (expt x 2))) '(+ (* 3 (expt x 2)) (* 2 (expt x 1))))
        (test (derivative '(expt 5 (expt x 2))) '(* (* 2 (expt x 1)) (expt 5 (expt x 2)) (ln 5)))
        (test (derivative '(/ 3 (* 2 (expt x 2)))) '(/ (- (* 0 (* 2 (expt x 2))) (* 3 1)) (expt (* 2 (expt x 2)) 2)))
        (test (derivative '(/ 3 x)) '(/ (- (* 0 x) (* 3 1)) (expt x 2)))
        (test (derivative '(* 2 (* (sin x) (cos x)))) '(* 2 (+ (* (cos x) (cos x)) (* (sin x) (- (sin x))))))
        (test (derivative '(* 2 (* (expt e x) (* (sin x) (cos x))))) '(* 2 (+ (* (expt e x) (* (sin x) (cos x))) (* (expt e x) (+ (* (cos x) (cos x)) (* (sin x) (- (sin x))))))))
        (test (derivative '(cos (* 2 (expt x 2)))) '(* (* 2 (* 2 (expt x 1))) (- (sin (* 2 (expt x 2))))))
        (test (derivative '(sin (ln (expt x 2)))) '(* (* (* 2 (expt x 1)) (/ 1 (expt x 2))) (cos (ln (expt x 2)))))
        (test (derivative '(+ (sin (* 2 x)) (cos (* 2 (expt x 2))))) '(+
                                                                       (* (* 2 1) (cos (* 2 x)))
                                                                       (*
                                                                        (* 2 (* 2 (expt x 1)))
                                                                        (- (sin (* 2 (expt x 2)))))))
        (test (derivative '(* (sin (* 2 x)) (cos (* 2 (expt x 2))))) '(+
                                                                       (*
                                                                        (* (* 2 1) (cos (* 2 x)))
                                                                        (cos (* 2 (expt x 2))))
                                                                       (*
                                                                        (sin (* 2 x))
                                                                        (*
                                                                         (* 2 (* 2 (expt x 1)))
                                                                         (- (sin (* 2 (expt x 2))))))))
              )
        )
  (derivative (list '* 'x 'x))
  (derivative '(* 2 (expt x 5)))
  (derivative '(expt x 10))
  (run-tests the-tests)






                            
  