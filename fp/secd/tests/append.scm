(letrec 
 ((append 
   (lambda (xs ys)
     (if (eq? xs '())
         ys
         (cons (car xs) (append (cdr xs) ys))))))
 (display (append '(1 2 3) '(4 5 6))))
