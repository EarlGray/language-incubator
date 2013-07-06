(letrec (
  (range (lambda (n)
     (if (eq? n 0) 
          '()
          (append (range (- n 1)) (list n))))))

  (begin
    (display (range 6))
    (newline)))
