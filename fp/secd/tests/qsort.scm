(letrec (
  (unzip-with (lambda (pred xs)
    (if (eq? xs '())
        (list '() '())
        (letrec ((hd (car xs))
                 (r (unzip-with pred (cdr xs)))
                 (r-pos (car r))
                 (r-neg (cadr r)))
          (if (pred hd)
              (list (cons hd r-pos) r-neg)
              (list r-pos (cons hd r-neg)))))))

  (quicksort 
    (lambda (xs)
      (let ((n (length xs)))
        (cond 
          ((eq? n 0) xs)
          ((eq? n 1) xs)
          (else
            (letrec ((pivot (car xs))
                     (r (unzip-with (lambda (x) (< x pivot)) (cdr xs))))
              (append (car r) (list pivot) (cadr r))))))))
  )
  (let ((inp (read)))
    (begin 
      ;(display (quicksort inp))
      (display (unzip-with (lambda (x) (< x 10000)) inp))
      (newline))))
