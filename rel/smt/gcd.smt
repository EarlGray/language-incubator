(set-option :produce-models true)
;(set-logic QF_LIA)

(define-fun x () Int 15)  ; x: Int = 15
(define-fun y () Int 12)  ; y: Int = 12
(declare-fun k () Int)

(assert
  (exists ((m Int) (n Int)) 
    (and
      (= x (* k m))       ; so k is a divisor of x
      (= y (* k n))       ; so k is a divisor of y
      ; and there is no k1 > k that is a divisor of both x and y
      (not (exists ((k1 Int) (m1 Int) (n1 Int))
              (and (< k k1) (= x (* k1 m1)) (= y (* k1 n1))))))))
          
(check-sat)
(get-value (k))
