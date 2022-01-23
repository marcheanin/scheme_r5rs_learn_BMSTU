(load "unit-test.scm")

'feature-if-else
'feature-nested-if
;'feature-repeat-loop
;'feature-break-continue
;'feature-switch-case

(define (interpret program stack)
  (letrec
      ((return-stack stack)
       (words '())
       (varaibles '())
       (put1 (lambda (x)
               (set! stack (cons x stack))
               ))
       (peak1 (lambda ()
                (car stack))) ;not used
       (peak2 (lambda ()
                (if (null? (car stack))
                    (cadr stack)
                    (if (null? (cdar stack))
                        (list (caar stack))
                        (list (caar stack) (cadar stack))))) ;not used
              )
       (format-logic (lambda (a)
                       (if a -1 0)))
       (op= (lambda (a b)
              (format-logic (= a b))))
       (op< (lambda (a b)
              (format-logic (< a b))))
       (op> (lambda (a b)
              (format-logic (> a b))))
       (true? (lambda (a)
                (not (= a 0))))
       (op-not (lambda (a)
                 (format-logic (not (true? a)))))
       (op-and (lambda (a b)
                 (format-logic (and (true? a) (true? b)))))
       (op-or (lambda (a b)
                (format-logic (or (true? a) (true? b)))))
       
       (skip (lambda (i balance beg end)
               (if (or
                    (>= i (vector-length program))
                    (= balance 0))
                   (- i 1)
                   (begin
                     (if (equal? (vector-ref program i) end) (set! balance (- balance 1)))
                     (if (equal? (vector-ref program i) beg) (set! balance (+ balance 1)))
                     (skip (+ 1 i) balance beg end)))))
       (skip-if (lambda (i balance)
                  (if (or
                       (>= i (vector-length program))
                       (= balance 0))
                      (- i 1)
                      (begin
                        (if (equal? (vector-ref program i) 'endif) (set! balance (- balance 1)))
                        (if (and (= balance 1) (equal? (vector-ref program i) 'else)) (set! balance (- balance 1)))
                        (if (equal? (vector-ref program i) 'if) (set! balance (+ balance 1)))
                        (skip-if (+ 1 i) balance)))))
       (skip-switch (lambda (i case)
                          (if (>= i (vector-length program))
                              (- i 1)
                              (if (or (equal? (vector-ref program i) 'endswitch)
                                      (and (equal? (vector-ref program i) 'case)
                                           (equal? (vector-ref program (+ i 1)) case)))
                                  (if (equal? (vector-ref program i) 'endswitch)
                                      i
                                      (+ 1 i))
                                  (if (equal? (vector-ref program i) 'switch)
                                      ( + 1 (skip (+ 1 i) 1 'switch 'endswitch))
                                      (skip-switch (+ 1 i) case))))))
       
       (interpret-symbol (lambda (i)
                           (if (< i (vector-length program))
                               (let ((symbol (vector-ref program i)))
                                 (cond
                                   ;number
                                   ((number? symbol) (put1 symbol))

                                   ;arithmetic
                                   ((equal? symbol '+) (set! stack (cons (+ (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol '-) (set! stack (cons (- (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol '*) (set! stack (cons (* (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol '/) (set! stack (cons (quotient (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol 'mod) (set! stack (cons (remainder (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol 'neg) (set! stack (cons (- (car stack)) (cdr stack))))

                                   ;compare
                                   ((equal? symbol '=) (set! stack (cons (op= (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol '>) (set! stack (cons (op> (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol '<) (set! stack (cons (op< (cadr stack) (car stack)) (cddr stack))))

                                   ;logic
                                   ((equal? symbol 'not) (set! stack (cons (op-not (car stack)) (cdr stack))))
                                   ((equal? symbol 'and) (set! stack (cons (op-and (cadr stack) (car stack)) (cddr stack))))
                                   ((equal? symbol 'or) (set! stack (cons (op-or (cadr stack) (car stack)) (cddr stack))))

                                   ;stack operations
                                   ((equal? symbol 'drop) (set! stack (cdr stack)))
                                   ((equal? symbol 'swap) (set! stack (cons (cadr stack) (cons (car stack) (cddr stack)))))
                                   ((equal? symbol 'dup) (set! stack (cons (car stack) stack)))
                                   ((equal? symbol 'over) (set! stack (cons (cadr stack) stack)))
                                   ((equal? symbol 'rot) (set! stack (cons (caddr stack) (cons (cadr stack) (cons (car stack) (cdddr stack))))))
                                   ((equal? symbol 'depth) (set! stack (cons (length stack) stack)))

                                   ;if
                                   ((equal? symbol 'if)
                                    (begin
                                      (if (not (true? (car stack))) (set! i (skip-if (+ 1 i) 1)))
                                      (set! stack (cdr stack))))
                                   ((equal? symbol 'else) (set! i (skip-if (+ 1 i) 1)))

                                   ;define
                                   ((equal? symbol 'define)
                                    (begin
                                      (set! i (+ 1 i))
                                      (set! words (cons (list (vector-ref program i) i) words))
                                      (set! i (skip (+ 1 i) 1 'define 'end))))
                                   ((assq symbol words)
                                    (begin
                                      (set! return-stack (cons i return-stack))
                                      (set! i (cadr (assq symbol words)))))
                                   ((or (equal? symbol 'end) (equal? symbol 'exit) (equal? symbol 'endlambda))
                                    (begin
                                      (set! i (car return-stack))
                                      (set! return-stack (cdr return-stack))
                                      (if (equal? symbol 'endlambda)
                                          (begin (set! i (car return-stack))
                                                 (set! return-stack (cdr return-stack))))))

                                   ;untill
                                   ((equal? symbol 'repeat) (begin
                                                              (if (not (true? (car stack)))
                                                                  (begin
                                                                    (set! return-stack
                                                                          (cons
                                                                           (+ 1 (skip (+ 1 i) 1 'repeat 'until))
                                                                           return-stack)))
                                                                  (begin
                                                                    (set! return-stack (cons i return-stack))))))
                                   ((or (equal? symbol 'until))
                                    (begin
                                      (if (equal? i (- (car return-stack) 1))
                                          (set! stack (cdr stack)))
                                      (set! i (- (car return-stack) 1))                                          
                                      (set! return-stack (cdr return-stack))))
                                   ;continue
                                   ((or (equal? symbol 'continue))
                                    (begin
                                      (set! i (- (car return-stack) 1))
                                      (if (equal? (vector-ref program i) 'until)
                                          (set! stack (cdr stack)))
                                      (if (not (equal? (vector-ref program i) 'lambda))
                                          (set! return-stack (cdr return-stack)))))

                                   ;break
                                   ((or (equal? symbol 'break))
                                    (begin
                                      (set! stack (cons 0 (cdr stack)))
                                      (set! i (- (car return-stack) 1))
                                      (set! return-stack (cdr return-stack))))
                                   ;switch
                                   ((equal? symbol 'switch)
                                        (begin
                                          (set! i (skip-switch (+ i 1) (car stack)))
                                          (set! stack (cdr stack))
                                          ))
                                       ((equal? symbol 'exitcase)
                                        (set! i (skip (+ i 1) 1 'switch 'endswitch)))
                                   ((equal? symbol 'lambda)
                                        (begin
                                          (set! stack (cons (list (+ i 0)) stack))
                                          (set! i (skip (+ 1 i) 1 'lambda 'endlambda))))
                                       ((equal? symbol 'do)
                                        (begin
                                          (set! return-stack (cons i return-stack))
                                          (set! i (caar stack))
                                          (set! return-stack (cons (+ i 1) return-stack))
                                          (set! stack (cdr stack))
                                          ))
                                   )
                                 
                                   
                                 ;(display stack) (newline)  
                                 (interpret-symbol (+ i 1)))                          
                               )
                           )
                         )
       )
    (begin
      (interpret-symbol 0)
      stack))
  )

(define tests (list
               (test (interpret #(2 3 * 4 5 * +) '()) '(26))
               (test (interpret #(neg) '(1)) '(-1))
               (test (interpret #(10 2 /) '()) '(5))
               (test (interpret #(10 2 >) '()) '(-1))
               (test (interpret #(10 2 <) '()) '(0))
               (test (interpret #(10 5 + 15 =) '()) '(-1))
               (test (interpret #(1 1 and) '()) '(-1))
               (test (interpret #(1 0 and) '()) '(0))
               (test (interpret #(1 0 or) '()) '(-1))
               (test (interpret #(1 2 3 drop) '()) '(2 1))
               (test (interpret #(1 2 3 4 5 swap) '()) '(4 5 3 2 1))
               (test (interpret #(1 2 3 4 5 dup) '()) '(5 5 4 3 2 1))
               (test (interpret #(1 2 3 4 5 over) '()) '(4 5 4 3 2 1))
               (test (interpret #(1 2 3 4 5 rot) '()) '(3 4 5 2 1))
               (test (interpret #(depth 0 0 0 depth 0 0 0 depth) '()) '(8 0 0 0 4 0 0 0 0))
               (test (interpret #(1 dup if dup dup if 5 endif endif 3) '()) '(3 5 1 1))
               (test (interpret #(0 dup if dup dup if 5 endif endif 3) '()) '(3 0))
               (test (interpret #(   define abs 
                                      dup 0 < 
                                      if neg endif 
                                      end 
                                      abs    ) 
                                '(-9)) '(9))
               (test (interpret #(   define =0? dup 0 = end
                                      define <0? dup 0 < end
                                      define signum
                                      =0? if exit endif
                                      <0? if drop -1 exit endif
                                      drop
                                      1
                                      end
                                      0 signum
                                      -5 signum
                                      10 signum       ) (quote ())) '(1 -1 0))
               (test (interpret #(   define -- 1 - end
                                      define =0? dup 0 = end
                                      define =1? dup 1 = end
                                      define factorial
                                      =0? if drop 1 exit endif
                                      =1? if drop 1 exit endif
                                      dup --
                                      factorial
                                      *
                                      end
                                      0 factorial
                                      1 factorial
                                      2 factorial
                                      3 factorial
                                      4 factorial     ) (quote ())) '(24 6 2 1 1))
               (test (interpret #(   define =0? dup 0 = end
                                      define =1? dup 1 = end
                                      define -- 1 - end
                                      define fib
                                      =0? if drop 0 exit endif
                                      =1? if drop 1 exit endif
                                      -- dup
                                      -- fib
                                      swap fib
                                      +
                                      end
                                      define make-fib
                                      dup 0 < if drop exit endif
                                      dup fib
                                      swap --
                                      make-fib
                                      end
                                      10 make-fib     ) (quote ())) '(0 1 1 2 3 5 8 13 21 34 55))
               (test (interpret #(   define =0? dup 0 = end
                                      define gcd
                                      =0? if drop exit endif
                                      swap over mod
                                      gcd
                                      end
                                      90 99 gcd
                                      234 8100 gcd    ) '()) '(18 9))
               (test (interpret #(1 if 100 else 200 endif) '()) '(100))
               (test (interpret #(0 if 100 else 200 endif) '()) '(200))
               (test (interpret #(0 if 1 if 2 endif 3 endif 4) '()) '(4))
               (test (interpret #(1 if 2 if 3 endif 4 endif 5) '()) '(5 4 3))
               (test (interpret #(1 if 0 if 2 endif 3 endif 4) '()) '(4 3))
               (test (interpret #(3 repeat 1 swap 1 - until) '()) '(1 1 1 1))
               (test (interpret #(2 10 repeat 1 - dup 2 mod if continue endif swap 2 * swap until) '()) '(64))
               (test (interpret #(2 8 repeat 1 - dup 2 mod not if continue endif swap 2 * swap until) '()) '(64))
               (test (interpret #(dup 3 mod
                                      switch
                                      case 0 0 exitcase
                                      case 1 1 exitcase
                                      case 2
                                      switch
                                      case 2 0 exitcase
                                      endswitch
                                      endswitch
                                      ) '(17)) '())
               (test (interpret #(dup 3 mod
                                      switch
                                      case 0 0 exitcase
                                      case 1 1 exitcase
                                      case 2
                                      switch
                                      case 2 0 exitcase
                                      endswitch
                                      endswitch
                                      ) '(2)) '(0))
               )
  )

(run-tests tests)
                


      

               

                           
