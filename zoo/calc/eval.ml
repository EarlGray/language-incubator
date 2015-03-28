open Syntax

let rec eval = function
    | Numeral n -> n
    | Plus (e1, e2) -> eval e1 + eval e2
    | Minus (e1, e2) -> eval e1 - eval e2
    | Times (e1, e2) -> eval e1 * eval e2
    | Divide (e1, e2) ->
        let n2 = eval e2 in
            if n2 <> 0 then eval e1 / n2 else failwith "Division by zero"
    | Negate e -> - (eval e)

