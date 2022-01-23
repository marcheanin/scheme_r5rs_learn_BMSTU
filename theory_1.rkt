(define counter
  (let ((n 0))
    (set! n (+ n 1))
    n))
(list counter counter)