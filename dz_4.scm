;#for from lab_4
(define-syntax for
  (syntax-rules (in as)
    ((for x in xs . actions) (letrec ((loop (lambda (xs1)
                                              (if (not (null? xs1))
                                                  (let ((x (car xs1)))
                                                    (begin (begin . actions) (loop (cdr xs1))))
                                                  )
                                              )))
                               (loop xs))
                             )
    ((for xs as x . actions) (for x in xs . actions))
    )
  )

;#1
(define (fact x)
  (if (< x 2)
      1
      (* x (memoized-factorial (- x 1)))))

  
(define memoized-factorial
  (let ((known-results '()))
    (lambda (x)
      (let* ((xs x)
             (res (assoc xs known-results)))
        (if res
            (cadr res)
            (let ((res (fact x)))
              (set! known-results (cons (list xs res) known-results))
              res))))))


(begin
  (display (memoized-factorial 5)) (newline)
  (display (memoized-factorial 50)) (newline)
  )

;#2
(define-syntax lazy-cons
  (syntax-rules ()
    ((lazy-cons a b)
     (cons a (list (delay b))))))

(define (lazy-car p) (car p))

(define (lazy-cdr p)
  (force (cadr p)))

(define (lazy-head xs k)
  (define (loop xs i k)
    (if (< i k)
        (cons (lazy-car xs) (loop (lazy-cdr xs) (+ 1 i) k))
        '()))
  (loop xs 0 k))

(define (lazy-ref xs k)
  (define (loop xs i k)
    (if (< i k)
        (loop (lazy-cdr xs) (+ i 1) k)
        (lazy-car xs)))
  (loop xs 1 k))

(define (naturals start)
  (lazy-cons start (naturals (+ 1 start))))

(define (generate-factorials)
  (define (loop n n!)
    (lazy-cons n! (loop (+ n 1) (* n! (+ n 1)))))
  (loop 1 1))

(define (lazy-factorial n)
  (lazy-ref (generate-factorials) n))

(display (lazy-head (naturals 10) 12)) (newline)

(begin
  (display (lazy-factorial 10)) (newline)
  (display (lazy-factorial 50)) (newline))

;#3
(define (read-words)
  (define (loop xs)
    (let ((x (read-char)))
      (cond ((eof-object? x) (if (null? xs)
                                 xs
                                 (list (list->string xs))))
            ((char-whitespace? x) (if (null? xs)
                                      (loop xs)
                                      (cons (list->string xs) (loop '()))))
            (else (loop (append xs (list x)))))))
    
  (loop '()))

;#4

(define (s->s s)
  (if (symbol? s)
      (symbol->string s)
      (string->symbol s)))

(define-syntax define-struct
  (syntax-rules ()
    ((_ name (fields ... ))
     (begin
       ; CONSTRUCTOR
       (eval
        (list 'define (list
                       (s->s (string-append "make-" (s->s 'name)))
                       'fields ...)
              (list 'list
                    (s->s 'name)
                    (list 'map 'list
                          (cons 'list (map s->s '(fields ...)))
                          (cons 'list '(fields ...)))))
        (interaction-environment))
       ; PREDICATE
       (eval
        (list 'define (list
                       (s->s (string-append (s->s 'name)  "?"))
                       'struct)
              (list 'and
                    (list 'pair? 'struct)
                    (list 'equal? (list 'car 'struct) (s->s 'name))))
        (interaction-environment))
       (let ((fields-stringed (map s->s '(fields ...))))
         ; REF
         (for field in fields-stringed
           (eval
            (list 'define (list
                           (s->s (string-append (s->s 'name) "-" field))
                           'struct)
                  (list 'cadr (list 'assq field (list 'cadr 'struct))))
            (interaction-environment)))
         ; SET
         (for field in fields-stringed
           (eval
            (list 'define (list
                           (s->s (string-append "set-" (s->s 'name) "-" field "!"))
                           'struct
                           'val)
                  (list 'set-car! (list 'cdr (list 'assq field (list 'cadr 'struct))) 'val))
            (interaction-environment))))))))

;#5
(define (my-eval exprs)
  (eval exprs (interaction-environment)))

(define-syntax define-data
  (syntax-rules ()
    ((define-data data-name ((name field1 ...) ...))
     (begin
       (my-eval (list 'define
                      'name
                      (lambda (field1 ...)
                        (list (list 'd-name 'data-name) (list 't-name 'name)
                              (list 'field1 field1) ...)))) ...
       (my-eval (list 'define
                      (s->s (string-append (s->s 'data-name) "?"))
                      (lambda (x)
                        (and (list? x) (>= (length x) 2)
                             (let ((d-nameres (assoc 'd-name x)))
                               (and d-nameres (equal? (cadr d-nameres) 'data-name)))))))))))

(define-syntax match
  (syntax-rules ()
    ((match x ((name field1 ...) expr) ...)
       (cond
         ((equal? (cadadr x) 'name)
           (let ((field1 (cadr (assoc 'field1 x))) ...)
             expr))
          ...
          (else x)))))


; TESTS

; Определяем тип
;
(define-data figure ((square a)
                     (rectangle a b)
                     (triangle a b c)
                     (circle r)))

; Определяем значения типа
;
(define s (square 10))
(define r (rectangle 10 20))
(define t (triangle 10 20 30))
(define c (circle 10))

; Пусть определение алгебраического типа вводит
; не только конструкторы, но и предикат этого типа:
;
(display (and (figure? s)
              (figure? r)
              (figure? t)
              (figure? c))) (newline)

(define pi (acos -1)) ; Для окружности
  
(define (perim f)
  (match f 
    ((square a)       (* 4 a))
    ((rectangle a b)  (* 2 (+ a b)))
    ((triangle a b c) (+ a b c))
    ((circle r)       (* 2 pi r))))
  
(display (perim s)) (newline)
(display (perim r)) (newline)
(display (perim t)) (newline)
(display (perim c)) (newline)