(letrec
;; what:
(
(compile-form (lambda (f)
  (let ((hd (car f))
        (tl (cdr f)))
    (cond
      ((eq? hd 'quote)
        (list 'LDC (car tl)))
      ((eq? hd '+)
        (append (compile (car tl)) (compile (cadr tl)) '(ADD)))
      ((eq? hd '*)
        (append (compile (car tl)) (compile (cadr tl)) '(MUL)))
      ((eq? hd 'atom?)
        (append (compile (car tl)) '(ATOM)))
      ((eq? hd 'car)
        (append (compile (car tl)) '(CAR)))
      ((eq? hd 'cdr)
        (append (compile (car tl)) '(CDR)))
      ((eq? hd 'cons)
        (append (compile (car tl)) (compile (cadr tl)) '(CONS)))
      ((eq? hd 'eq? )
        (append (compile (car tl)) (compile (cadr tl)) '(EQ)))
      ((eq? hd 'if )
        (let ((condc (compile (car tl)))
              (thenb (append (compile (cadr tl)) '(JOIN)))
              (elseb (append (compile (caddr tl)) '(JOIN))))
          (append condc '(SEL) (list thenb) (list elseb))))
      ((eq? hd 'lambda)
        (let ((args (car tl))
              (body (append (compile (cadr tl)) '(RTN))))
          (list 'LDF (list args body))))
      ;((eq? hd 'let)
      ;  ())
      (else '(TODO))
    ))))

(compile (lambda (s)
  (cond
    ((symbol? s) (list 'LD s))
    ((number? s) (list 'LDC s))
    (else (compile-form s)))))

(loop (lambda (body)
   (begin
     (body) (loop body))))
)

;; <let> in
(loop (lambda ()
    (display (compile (read)))
    (newline)))
)
