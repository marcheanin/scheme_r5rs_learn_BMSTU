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
