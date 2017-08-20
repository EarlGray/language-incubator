theory Primitives
imports Main
begin

(* bool *)
datatype bool = True | False

fun conj :: "bool \<Rightarrow> bool \<Rightarrow> bool" where
"conj True True = True" |
"conj _    _    = False"

(* nat *)
datatype nat = Z | Suc nat

definition zero :: nat where "zero = Z"
definition one :: nat where "one = Suc zero"
definition two :: nat where "two = Suc one"
definition three :: nat where "three = Suc two"


fun add :: "nat \<Rightarrow> nat \<Rightarrow> nat" where
"add Z       n = n" |
"add (Suc m) n = Suc (add m n)"

lemma add_mZ [simp]: "add m Z = m"
apply(induction m)
apply(auto)
done

(* list *)
datatype 'a list = Nil | Cons 'a "'a list"

fun app :: "'a list \<Rightarrow> 'a list \<Rightarrow> 'a list" where
"app Nil         ys = ys" |
"app (Cons x xs) ys = Cons x (app xs ys)"

fun rev :: "'a list \<Rightarrow> 'a list" where
"rev Nil = Nil" | 
"rev (Cons x xs) = app (rev xs) (Cons x Nil)"

value "rev (Cons 1 (Cons 2 (Cons 3 Nil)))"
value "rev (Cons a (Cons b Nil))"

lemma app_Nil [simp]: "app xs Nil = xs"
apply(induction xs)
apply(auto)
done

lemma app_assoc [simp]: "app (app xs ys) zs = app xs (app ys zs)"
apply(induction xs)
apply(auto)
done

lemma rev_app [simp]: "rev (app xs ys) = app (rev ys) (rev xs)"
apply(induction xs)
apply(auto)
done

theorem rev_rev [simp]: "rev (rev xs) = xs"
apply(induction xs)
apply(auto)
done

(*
predefined list notation:
[]        for Nil
x # xs    for (Cons x xs)
[x1, x2, ..., xN]
xs @ ys   for (app xs ys)
*)

fun length :: "'a list \<Rightarrow> nat" where
"length Nil = Z" | 
"length (Cons _ xs) = Suc (length xs)"

fun map :: "('a \<Rightarrow> 'b) \<Rightarrow> 'a list \<Rightarrow> 'b list" where
"map f Nil = Nil" |
"map f (Cons x xs) = Cons (f x) (map f xs)"

fun head :: "'a list \<Rightarrow> 'a" where
"head (Cons x xs) = x"

fun tail :: "'a list \<Rightarrow> 'a list" where
"tail (Cons _ xs) = xs"

(*
 *  Exercises
 *)

value "let one = Suc Z; two = Suc one in add one two"

lemma add_assoc [simp]: "add (add x y) z = add x (add y z)"
apply(induction x)
apply(auto)
done

lemma add_one [simp]: "add x (Suc Z) = Suc x"
apply (induction x)
apply auto
done

lemma add_assoc_1 [simp]: "Suc (add y x) = add y (Suc x)"
apply (induction y)
apply auto
done


lemma add_comm [simp]: "add x y = add y x"
apply(induction x)
apply auto
done


fun double :: "nat \<Rightarrow> nat" where
"double Z = Z" | 
"double (Suc m) = Suc (Suc (double m))"

theorem double_sum : "double m = add m m"
apply (induction m)
apply auto
done


fun snoc :: "'a list \<Rightarrow> 'a \<Rightarrow> 'a list" where
"snoc Nil v = Cons v Nil" | 
"snoc (Cons x xs) v = Cons x (snoc xs v)"

fun reverse :: "'a list \<Rightarrow> 'a list" where
"reverse Nil = Nil" |
"reverse (Cons x xs) = snoc (reverse xs) x"

(*
theorem reverse_reverse : "reverse (reverse xs) = xs"
apply (induction xs)
apply auto
*)

(*
abbreviation sq' :: "Main.nat \<Rightarrow> Main.nat" where
"sq' n \<equiv> n * n"
*)

end
