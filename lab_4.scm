;(use-syntax (ice-9 syncase))
;#1
(define call/cc call-with-current-continuation)


(define-syntax use-assertions
  (syntax-rules ()
    ((use-assertions)
     (begin
       (eval `(define r #t) (interaction-environment))
       (eval `(call/cc (lambda (c) (set! r c))) (interaction-environment))
       )
     )
    )
  )

(define-syntax assert
  (syntax-rules ()
    ((assert func)
     (begin
       (let ((res func))
         (if (and res)
             #t
             (begin
               (display "FAILED: ")
               (r (display (quote func)))
               )
             )
         )
       )
     )
    )
  )

(use-assertions)

(define (1/x x)
  (assert (not (zero? x)))
  (/ 1 x))



(map 1/x '(1 2 3 4 5)) 
(map 1/x '(-2 -1 0 1 2))
(newline)

;#2
(define (load-data file)
  (call-with-input-file file (lambda (port) (read port)))
  )

(define (save-data data file)
  (call-with-output-file file (lambda (port) (write data port)))
  )
    
(define data (load-data "input.txt"))
(display data)
(newline)
;(save-data data "output.txt")


(define (count-line file)
  (call-with-input-file file
    (lambda (port)
      (define str1 "")
      (define str2 "")
      (define (read-loop count)
        (set! str1 str2)
        (set! str2 (read-char port))
        (if (eof-object? str2)
            count
            (if (or (and (eq? str2 #\return) (not (eq? str1 #\newline))) (and (eq? str2 #\newline) (not (eq? str1 #\newline)) (not (eq? str1 #\return))))
                (read-loop (+ count 1))
                (read-loop count)
                )
            )
        )
      (read-loop 0)
      ))
  )

(count-line "lab_4.scm")

;#3
(define (trib n)
  (if (<= n 1)
      0
      (if (= n 2)
          1
          (+ (trib (- n 1)) (trib (- n 2)) (trib (- n 3)))
          )
      )
  )

(define memory '(((trib-memory 0) . 0)
                 ((trib-memory 1) . 0)
                 ((trib-memory 2) . 1)
                 )
  )

(define (trib-memory n)
  (or (letrec ((loop (lambda (m)
                       (and (not (null? m))
                            (if (equal? `(trib-memory ,n) (caar m))
                                (cdar m)
                                (loop (cdr m))
                                )
                            )
                       )
                     )
               )
        (loop memory))
      (let ((rez (+ (trib-memory (- n 1))
                    (trib-memory (- n 2))
                    (trib-memory (- n 3))
                    )))
        (begin (set! memory (cons `((trib-memory ,n) . ,rez) memory))
               rez)
        )
      )
  )
(trib-memory 10)

;#4
(define-syntax my-if
  (syntax-rules ()
    ((my-if condition if_t if_f)
     (force (or (and condition
                     (delay if_t))
                (delay if_f)))
     )
    )
  )
(my-if #t 1 (/ 1 0))
(my-if #f (/ 1 0) 1)

;#5
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((value mean)) action)
     ((lambda (value) action) mean))
    ((my-let ((value mean) . assig) action)
     (my-let assig ((lambda (value) action) mean)))
    )
  )

(my-let ((x 2)) x)
(my-let ((x 2) (y 3) (z 4)) (+ x y z))

(define-syntax my-let*
  (syntax-rules ()
    ((my-let* ((value mean)) action)
     ((lambda (value) action) mean))
    ((my-let* ((value mean) . assig) action)
     ((lambda (value) (my-let* assig action)) mean))
    )
  )

(my-let* ((x 2) (y (+ x 2))) y)

;#6

;A
(define-syntax when
  (syntax-rules ()
    ((when condition action) (and (quote condition) action))                         
    ((when condition . actions) (and condition (begin . actions)))
    )
  )
(define x 2)
(when (> x 0) (display "x > 0")  (newline))

(define-syntax unless
  (syntax-rules ()
    ((unless condition action) (if (not (quote condition)) action))                         
    ((unless condition . actions) (if (not condition) (begin . actions)))
    )
  )
(unless (= x 0) (display "x != 0") (newline))

;Б
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

(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))
(newline)

;B
(define-syntax while
  (syntax-rules ()
    ((while condition . actions)
     (letrec ((loop (lambda ()
                      (if (and condition)
                          (begin (begin . actions) (loop))
                          )
                      )
                    )) (loop))
     )
    )
  )
(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))

(newline)
;Г
(define-syntax repeat
  (syntax-rules (until)
    ((repeat actions until condition) (letrec ((loop (lambda ()
                                                       (begin (begin . actions)
                                                              (if (not (and condition))
                                                                  (loop)
                                                                  )
                                                              )
                                                       ))
                                               ) (loop)) 
                                      )
    )
  )
(let ((i 0)
      (j 0))
  (repeat ((set! j 0)
           (repeat ((display (list i j))
                    (set! j (+ j 1)))
                   until (= j 3))
           (set! i (+ i 1))
           (newline))
          until (= i 3)))
(newline)
;Д
(define-syntax cout
  (syntax-rules (<<)
    ((cout . actions) (letrec ((loop (lambda (actions1)
                                       (if (null? actions1)
                                           (display "")
                                           (begin
                                             (let ((action (cadr actions1)))
                                               (if (eq? action 'endl)
                                                   (newline)
                                                   (display action)))
                                             (loop (cddr actions1)))
                                           )
                                       ))) (loop 'actions))
                      )
    )
  )

(cout << "a = " << 1 << endl << "b = " << 2 << endl)                   