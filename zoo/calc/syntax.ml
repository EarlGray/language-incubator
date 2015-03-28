type expression =
    | Numeral of int
    | Plus of expression * expression
    | Minus of expression * expression
    | Times of expression * expression
    | Divide of expression * expression
    | Negate of expression

let string_of_expression e =
    let rec to_str n e = 
        let (m, str) = match e with
            | Numeral n         -> (3, string_of_int n)
            | Negate e          -> (2, "-" ^ (to_str 0 e))
            | Times (e1, e2)    -> (1, (to_str 1 e1) ^ " * " ^ (to_str 2 e2))
            | Divide (e1, e2)   -> (1, (to_str 1 e1) ^ " / " ^ (to_str 2 e2))
            | Plus (e1, e2)     -> (1, (to_str 1 e1) ^ " + " ^ (to_str 2 e2))
            | Minus (e1, e2)    -> (1, (to_str 1 e1) ^ " - " ^ (to_str 2 e2))
        in if m < n then "(" ^ str ^ ")" else str
    in to_str (-1) e

