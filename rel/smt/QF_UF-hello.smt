; Run using cvc4:
; $ cvc4 --lang smt void.smt

(set-logic QF_UF)
(declare-fun p () Bool)
(assert (and p (not p)))
(check-sat)
(exit)
