(** [duplicate l] return if there is a duplicate (two identical elements) in a list l *)
let duplicate l =
    let rec aux = function
        | [] -> false
        | [e] -> false
        | e1::e2::q -> e1 = e2 || aux (e2::q) in
    aux (tri l)
