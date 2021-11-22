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
  (call-with-input-file file
    (lambda (port)
      (define str "")
      (define (read-loop xs)
        (set! str (read-char port))
        (if (eof-object? str)
            xs
            (read-loop (append xs (list str)))
            )
        )
      (list->string (read-loop '()))
      ))
  )

(define (save-data data file)
  (call-with-output-file file
    (lambda (port)
      (define (write-loop data)
        (if (null? data)
            #t
            (begin (display (car data) port) (write-loop (cdr data)))
            )
        )
      (write-loop (list data))
      )
    )
  )
    
(define data (load-data "input.txt"))
(display data)
(newline)
;(save-data data "output.txt")


(define (count-line file)
  (call-with-input-file file
    (lambda (port)
      (define str "")
      (define (read-loop count)
        (set! str (read-char port))
        (if (eof-object? str)
            count
            (if (or (eq? str #\newline) (eq? str #\return))
                (read-loop (+ count 1))
                (read-loop count)
            )
        )
        )
      (read-loop 0)
      ))
  )

(count-line "lab_4.scm")
