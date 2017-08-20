theory AExp
imports Main
begin

(* Syntax *)
type_synonym vname = string

datatype aexp
  = ANum int
  | AId vname
  | APlus aexp aexp

(* semantics *)
type_synonym val = int
type_synonym state = "vname \<Rightarrow> val"

fun aeval :: "aexp \<Rightarrow> state \<Rightarrow> val" where
"aeval (ANum n) s = n" |
"aeval (AId x) s = s x" | 
"aeval (APlus e1 e2) s = (aeval e1 s) + (aeval e2 s)"

value "aeval (APlus (ANum 2) (AId ''x'')) (\<lambda>_.2)"
value 
  "aeval 
    (APlus (ANum 2) (AId ''x'')) 
    (\<lambda>v.(case v of ''y'' \<Rightarrow> 2))"

end
