(define (prog-day x)
  (define a (cond ((or (and (= (remainder x 4) 0) (> (remainder x 100) 0)) (= (remainder x 400) 0)) 12)
                  (else 13)))
  (cond ((< x 2002) (display "не праздновался"))
        ((and (> x 2002) (< x 2009)) (display "праздновался неофициально \n"))
        ((< x 2021) (display "праздновался официально ") (display a) (display " сентября \n"))
        (else (display "будет праздноваться официально ") (display a) (display " сентября \n")))
  )


(prog-day 2003)
(prog-day 2010)
(prog-day 2012)
(prog-day 2032)