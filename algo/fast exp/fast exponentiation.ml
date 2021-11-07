(** [fast_exp a n] returns a**n with a low complexity *)
let rec fast_exp a n =
    if n = 0 then 1
    else let b = fast_exp a (n/2) in
        if n mod 2 = 0 then b*b
        else a*b*b
