(define (self-evaluating? expr)
  (or (number? expr) (string? expr)))
    

(define (variable? expr)
  (symbol? expr))

(define (quoted? expr) 
  )

(define (s-eval expr)
  (cond
    ((self-evaluating? expr) expr)
    ((variable? expr) (lookup-variable expr))
    ((quoted? expr) (quotation expr))
    ))
  
(define (s-apply func args)
  