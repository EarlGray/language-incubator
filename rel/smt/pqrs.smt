;; rlwrap z3 -smt2 -in
(declare-const p Bool)
(declare-const q Bool)
(declare-const r Bool)
(declare-const s Bool)

;; ((¬r ⇒ ¬p ∧ ¬q) ∨ s) ⇔ (p ∨ q ⇒ r ∨ s)
(define-fun lhs () Bool
  (or (=> (not r) (and (not p) (not q))) s))
(define-fun rhs () Bool
  (=> (or p q) (or r s)))

(define-fun conjecture () Bool
  (= lhs rhs))

(push)
(assert conjecture)
(check-sat)
;sat

(pop)
(assert (not conjecture))
(check-sat)
;unsat

;; `conjecture` is satisfiable and its negation is not,
;; so it's valid.

(exit)

