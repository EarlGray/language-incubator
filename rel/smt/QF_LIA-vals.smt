;; $ z3 -smt QF_LIA-vals.smt

(set-logic QF_LIA)
(set-option :produce-models true)

(declare-fun x () Int)
(declare-fun y () Int)

(assert (= (+ x (* 2 y)) 20))   ; x + 2y = 20
(assert (= (- x y) 2))          ; x - y  = 2

(check-sat)
(get-value (x y))
