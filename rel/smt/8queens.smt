(set-option :produce-models true)
(set-logic QF_LIA)

(define-fun n
  () Int
  8)

(define-fun compl
  ((x Int)) Int
  (- (- n 1) x))

(declare-fun x0 () Int)
(declare-fun y0 () Int)
(assert (and (<= 0 x0) (<= 0 y0) (< x0 n) (< y0 n)))

(declare-fun x1 () Int)
(declare-fun y1 () Int)
(assert (and (<= 0 x1) (<= 0 y1) (< x1 n) (< y1 n)))

(declare-fun x2 () Int)
(declare-fun y2 () Int)
(assert (and (<= 0 x2) (<= 0 y2) (< x2 n) (< y2 n)))

(declare-fun x3 () Int)
(declare-fun y3 () Int)
(assert (and (<= 0 x3) (<= 0 y3) (< x3 n) (< y3 n)))

(declare-fun x4 () Int)
(declare-fun y4 () Int)
(assert (and (<= 0 x4) (<= 0 y4) (< x4 n) (< y4 n)))

(declare-fun x5 () Int)
(declare-fun y5 () Int)
(assert (and (<= 0 x5) (<= 0 y5) (< x5 n) (< y5 n)))

(declare-fun x6 () Int)
(declare-fun y6 () Int)
(assert (and (<= 0 x6) (<= 0 y6) (< x6 n) (< y6 n)))

(declare-fun x7 () Int)
(declare-fun y7 () Int)
(assert (and (<= 0 x7) (<= 0 y7) (< x7 n) (< y7 n)))

; no queens on the same row
(assert (distinct x0 x1 x2 x3 x4 x5 x6 x7))

; no queens on the same column
(assert (distinct y0 y1 y2 y3 y4 y5 y6 y7))

; no queens on the same ascending diagonal
(assert (distinct (+ x0 y0) (+ x1 y1) (+ x2 y2) (+ x3 y3)
                  (+ x4 y4) (+ x5 y5) (+ x6 y6) (+ x7 y7)))

; no queens on the same descending diagonal
(assert (distinct (+ (compl x0) y0) (+ (compl x1) y1) (+ (compl x2) y2) (+ (compl x3) y3)
                  (+ (compl x4) y4) (+ (compl x5) y5) (+ (compl x6) y6) (+ (compl x7) y7)))

(check-sat)
(get-value (x0 y0 x1 y1 x2 y2 x3 y3 x4 y4 x5 y5 x6 y6 x7 y7))
