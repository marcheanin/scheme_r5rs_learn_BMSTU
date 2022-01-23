;#1
(define (my-char-upper-case? a)
  (and (<= (char->integer #\A) (char->integer a))
       (<= (char->integer a) (char->integer #\Z))))

(my-char-upper-case? #\S)

;#3
(define (remove-repeats xs)
  (if (null? xs)
      '()
      (if (member (car xs) (cdr xs))
          (remove-repeats (cdr xs))
          (cons (car xs) (remove-repeats (cdr xs))))))

(remove-repeats '(1 1 2 4 4 5 4 2 3 6 2 8))

;#5
(define (filter pr? xs)
  (if (null? xs)
      '()
      (if (pr? (car xs))
          (cons (car xs) (filter pr? (cdr xs)))
          (filter pr? (cdr xs)))))

(filter odd? '(1 2 3 4 5 6 7 8))

;#7
;a)
(define (my-map1 func xs)
  (if (null? xs)
      '()
      (cons (func (car xs)) (my-map1 func (cdr xs)))))

(my-map1 (lambda (x) (+ x 1)) '(1 2 3 4))
;b)
(define (my-map2 func . xss)
  (letrec ((len (length xss))
           (take-cars (lambda (xss1 i n xs)
                        (if (= i n)
                            xs
                            (take-cars xss1 (+ 1 i) n (append xs (list (car (list-ref xss1 i))))))))
           (cdrs (lambda (xss1 i n xss2)
                   (if (= i n)
                       xss2
                       (cdrs xss1 (+ i 1) n (append xss2 (list (cdr (list-ref xss1 i))))))))
           (loop (lambda (func xss1 xs)
                   (if (member '() xss1)
                       xs
                       (loop func (cdrs xss1 0 len '()) (append xs (list (apply func (take-cars xss1 0 len '())))))))))
    (loop func xss '())))

(my-map2 * '(1 2 3 4 5 5) '(5 6 7 8) '(9 0 2 3))
  
;#10
(define (my-list-ref xs i)
  (if (> i (- (length xs) 1))
      #f
      (if (eq? i 0)
          (car xs)
          (my-list-ref (cdr xs) (- i 1)))))

(my-list-ref '(1 2 3 4) 3)

(define (my-length xs)
  (if (null? xs)
      0
      (+ 1 (my-length (cdr xs)))))

(my-length '(1 2 3 4 5 6 7))

(define (my-reverse xs)
  (letrec ((loop (lambda (xs1 xs2)
                   (if (null? xs1)
                       xs2 
                       (loop (cdr xs1) (append (list (car xs1)) xs2))))))
    (loop xs '())))

(my-reverse '(1 2 3 4 5))

;#12
(define (count_pr pr? xs)
  (if (null? xs)
      0
      (if (pr? (car xs))
          (+ 1 (count_pr pr? (cdr xs)))
          (count_pr pr? (cdr xs)))))

(count_pr even? '(1 2 3 4 5 6 7 8))

;#13
(define (list-sum xs)
  (if (null? xs)
      0
      (+ (car xs) (list-sum (cdr xs)))))

(list-sum '(1 2 3 4 5))

(define (list-mul xs)
  (if (null? xs)
      1
      (* (car xs) (list-mul (cdr xs)))))

(list-mul '(1 2 3 4 5))

(define MAX_ELEM 1e9)
(define MIN_ELEM -1e9)

(define (list-min xs)
  (if (null? xs)
      MAX_ELEM
      (min (car xs) (list-min (cdr xs)))))

(list-min '(3 1 3 5 7))

(define (list-max xs)
  (if (null? xs)
      MIN_ELEM
      (max (car xs) (list-max (cdr xs)))))

(list-max '(3 1 3 5 7))

;#15

(define (recast xs)
  (if (and (= (length xs) 3) (list? (list-ref xs 1)) (list? (list-ref xs 2)) (= (length (list-ref xs 1)) 3) (= (length (list-ref xs 2)) 3))
      (let ((sign (list-ref xs 0))
            (sign1 (list-ref (list-ref xs 1) 0))
            (mul1 (list-ref (list-ref xs 1) 1))
            (sign2 (list-ref (list-ref xs 2) 0))
            (mul2 (list-ref (list-ref xs 2) 1))
            (op1 (list-ref (list-ref xs 1) 2))
            (op2 (list-ref (list-ref xs 2) 2)))
        (if (and (eq? sign '+) (eq? mul1 mul2) (eq? sign1 sign2) (eq? sign1 '*))
            `(* ,mul1 (+ ,op1 ,op2))
            #f))
      #f))

(recast '(+ (* 2 x) (* 2 y)))
(recast '(+ (* 2 (- a b)) (* 2 (- p q))))
(recast '(expt x 2))

;#17
(define (symbols-append . xs)
  (define (list-symbol->string xs)
    (cond ((null? xs) "")
          (else (string-append (symbol->string (car xs))
                               (list-symbol->string (cdr xs))))))
  (string->symbol (list-symbol->string xs)))

(symbols-append 'foo 'bar) ;⇒ foobar
(symbols-append 'foo '- 'bar) ;⇒ foo-bar

;#19
(define counter  0)
(define (fynction)
  (set! counter  (+ counter  1))
  counter)

(fynction)
(fynction)
(fynction)

;#24
(define (drop xs n)
  (if (or (= n 0) (null? xs))
      xs
      (drop (cdr xs) (- n 1))))

(drop '(1 2 3 4) 2)











                   
  
                             

