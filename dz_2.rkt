;----------------------#1-------------------------
;#1.1
(define (my-range a b d)
  (if (>= a b)
      '()
      (cons a (my-range (+ a d) b d))
      )
  )
(my-range 1 11 3)
(my-range 1 2 4)

;#1.2
(define (my-flatten xs)
  (if (null? xs)
      '()
      (if (list? (car xs))
          (append (my-flatten (car xs)) (my-flatten (cdr xs)))
          (cons (car xs) (my-flatten (cdr xs)))
          )
      )
  )
(my-flatten '((1) 2 (3 (4 5)) 6))

;#1.3
(define (my-element? x xs)
  (if (null? xs)
      #f
      (or (equal? x (car xs)) (my-element? x (cdr xs)))
      )
  )
(my-element? 1 '(3 2 1))
(my-element? 4 '(3 2 1))

;#1.4
(define (my-filter pred? xs)
  (if (null? xs)
      '()
      (if (pred? (car xs))
          (cons (car xs) (my-filter pred? (cdr xs)))
          (my-filter pred? (cdr xs))
          )
      )
  )
(my-filter odd? (my-range 0 10 1))
(my-filter (lambda (x) (= (remainder x 3) 0)) (my-range 0 13 1))

;#1.5
(define (my-fold-left op xs)
  (if (= (length xs) 1)
      (car xs)
      (my-fold-left op (cons (op (car xs) (car (cdr xs))) (cdr (cdr xs))))
      )
  )
(my-fold-left  quotient '(16 2 2 2 2))
(my-fold-left  quotient '(1))
(my-fold-left expt     '(2 3 4))

;#1.6
(define (my-fold-right op xs)
  (if (<= (length xs) 1)
      (car xs)
      (op (car xs) (my-fold-right op (cdr xs)))
      )
  )
(my-fold-right expt     '(2 3 4))
(my-fold-right expt     '(2))

;---------------#2---------------------

;#2.1
(define (list->set xs)
  (if (null? xs)
      '()
      (if (my-element? (car xs) (cdr xs))
          (list->set (cdr xs))
          (append (list->set (cdr xs)) (list (car xs)))
          )
      )
  )

(list->set '(1 1 2 3)) 

;#2.2
(define (set? xs)
  (if (null? xs)
      #t
      (and (my-element? (car xs) (cdr xs)) (set? (cdr xs)))
      )
  )

(set? '(1 2 3 3))                            
(set? '())

;#2.3
(define (union xs1 xs2)
  (if (null? xs1)
      (reverse xs2)
      (if (my-element? (car xs1) xs2)
          (union (cdr xs1) xs2)
          (union (cdr xs1) (cons (car xs1) xs2)))
      )
  )
  

(union '(1 2 3) '(2 3 4))

;#2.4
(define (intersection xs1 xs2)
  (if (null? xs1)
      '()
      (if (my-element? (car xs1) xs2)
          (cons (car xs1) (intersection (cdr xs1) xs2))
          (intersection (cdr xs1) xs2)
          )
      )
  )

(intersection '(1 2 3) '(2 3 4))

;#2.5
(define (difference xs1 xs2)
  (if (null? xs1)
      '()
      (if (my-element? (car xs1) xs2)
          (difference (cdr xs1) xs2)
          (cons (car xs1) (difference (cdr xs1) xs2))
          )
      )
  )
  
(difference '(1 2 3 4 5) '(2 3))

;#2.6
(define (symmetric-difference xs1 xs2)
  (union (reverse (difference xs1 xs2)) (difference xs2 xs1))
  )

(symmetric-difference '(1 2 3 4) '(3 4 5 6))

;#2.7
(define (set-eq? xs1 xs2)
  (null? (symmetric-difference xs1 xs2))
  )

(set-eq? '(1 2 3) '(3 2 1))                  
(set-eq? '(1 2) '(1 3))
(set-eq? '(1) '(1 2 3))

;----------------------#3------------------------

;#3.1

(define (string-trim-left str)
  (if (char-whitespace? (string-ref str 0) )
      (string-trim-left (substring str 1))
      str
      )
  )
(string-trim-left  "\t\tabc def")

;#3.2
(define (string-reverse str)
  (list->string (reverse (string->list str)))
  )

(define (string-trim-right str)
  ;(list->string (reverse (string->list (string-trim-left (list->string (reverse (string->list "wrrwr")))))))
  (string-reverse (string-trim-left (string-reverse str)))
  )

(string-trim-right "abc def\t")

;#3.3
(define (string-trim str)
  (string-trim-right (string-trim-left str))
  )

(string-trim       "\t abc def \n")

;#3.4
(define (string-prefix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((equal? a (substring b 0 (string-length a))) #t)
        (else #f)
        )
  )

(string-prefix? "abc" "abcdef")
(string-prefix? "bcd" "abcdef")
(string-prefix? "abcdef" "abc")
(newline)

;#3.5
(define (string-suffix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((equal? a (substring b (- (string-length b) (string-length a)))) #t)
        (else #f)
        )
  )

(string-suffix? "def" "abcdef")
(string-suffix? "bcd" "abcdef")
(newline)

;#3.6
(define (string-infix? a b)
  (cond ((> (string-length a) (string-length b)) #f)
        ((string-prefix? a b) #t)
        (else (string-infix? a (substring b 1)))
        )
  )

(string-infix? "def" "abcdefgh")
(string-infix? "abc" "abcdefgh")
(string-infix? "fgh" "abcdefgh")
(string-infix? "ijk" "abcdefgh")
(string-infix? "bcd" "abc")
(newline)

;#3.7

(define (make-elem a sep)
  (if (or (string-prefix? sep a) (= (string-length a) 0))
      ""
      (string-append (make-string 1 (string-ref a 0)) (make-elem (substring a 1) sep))
      )
  )

(define (string-split a sep)
  (if (= (string-length a) 0)
      '()
      (if (string-prefix? sep a)
          (string-split (substring a (string-length sep)) sep)
          (cons (make-elem a sep) (string-split (substring a (string-length (make-elem a sep))) sep))
          )
      )
  )

(string-split "x;y;z" ";")     
(string-split "x-->y-->z" "-->")
(string-split "qwe-WEF-EW-QDE" "-")
(string-split "abc;def;ghi" ";")
(newline)

;----------------------#4------------------------

;#4.1
;(define (make-multi-vector . xs)
;  (if (null? (car xs))
;      (if (= (length xs) 1)
;          0
;          (cadr xs)
;          )
;      (make-vector (car (car xs)) (make-multi-vector (cdr (car xs))))
;      )
;  )
;(define m (make-multi-vector '(11 12 9 16)))
;(define m1 (make-multi-vector '(2 2) 5))
;m1

(define (make-list xs)
  (if (null? xs)
      '()
      (cons 0 (make-list (cdr xs)))
      )
  )

(define (next-cord cord xs)
  (if (null? cord)
      '()
      (if (< (car cord) (- (car xs) 1))
          (cons (+ (car cord) 1) (cdr cord))
          (cons 0 (next-cord (cdr cord) (cdr xs)))
          )
      )
  )

(define (pr xs)
  (if (null? xs)
      1
      (* (car xs) (pr (cdr xs)))
      )
  )

(define (make-multi-vector . xs)
  (define sizes (car xs))
  (define fill (if (null? (cdr xs))
                   0
                   (car (cdr xs))
                   )
    )
    
  (define cord (make-list (car xs)))
  (define (loop i n)
    (if (= i n)
        '()
        (if (= i 0)
            (cons (cons fill (list (reverse cord))) (loop (+ i 1) n))
            (begin (set! cord (next-cord cord (reverse sizes))) (cons (cons fill (list (reverse cord))) (loop (+ i 1) n)))
            )
        )
    )
  (list->vector (loop 0 (pr sizes)))
  )
;(define m (make-multi-vector '(11 12 9 16)))
(define m (make-multi-vector '(3 3 3)))

;#4.2
(define (vector-size m)
  (length (vector->list m))
  )
(define (multi-vector? m)
  (list? (vector-ref m 0))
  )

(multi-vector? m)
(newline)

;#4.3
(define (multi-vector-set! m cord elem)
  (define (loop i n)
    (cond ((= i n) #f)
          ((equal? (car (cdr (vector-ref m i))) cord) (vector-set! m i (cons elem (list cord))))
          (else (loop (+ i 1) n))
          )
    )
  (loop 0 (vector-size m))
  )

(define (multi-vector-ref m cord)
  (define (loop i n)
    (cond ((= i n) #f)
          ((equal? (car (cdr (vector-ref m i))) cord) (car (vector-ref m i)))
          (else (loop (+ i 1) n))
          )
    )
  (loop 0 (vector-size m))
  )

(define m (make-multi-vector '(11 12 9 16)))
(multi-vector? m)
(multi-vector-set! m '(10 7 6 12) 'test)
(multi-vector-ref m '(10 7 6 12)) 

; Индексы '(1 2 1 1) и '(2 1 1 1) — разные индексы
(multi-vector-set! m '(1 2 1 1) 'X)
(multi-vector-set! m '(2 1 1 1) 'Y)
(multi-vector-ref m '(1 2 1 1)) 
(multi-vector-ref m '(2 1 1 1)) 

(define m (make-multi-vector '(3 5 7) -1))
(multi-vector-ref m '(0 0 0))

(define idxs
    (apply append
           (map (lambda (i)
                (apply append
                     (map (lambda (j)
                           (map (lambda (k)
                               (list i j k))
                               '(0 1 2))
                           '(0 1 2)))
                        '(0 1 2))))
                        
(define cube (make-multi-vector '(3 3 3)))

(begin
  (map (lambda (idx)
                  (multi-vector-set! cube idx idx))
              idxs)
  (map (lambda (idx)
                  (multi-vector-ref cube idx))
             idxs)))
  


  ;----------------------#5------------------------

  (define (f x) (+ x 2))
  (define (g x) (* x 3))
  (define (h x) (- x))

  (define (o . xs)
    (lambda (x)
      (if (null? xs)
          x
          ((car xs) ((apply o (cdr xs)) x)))))

  ((o f g h) 1)
  ((o f g) 1)
  ((o h) 1)
  ((o) 1)
  