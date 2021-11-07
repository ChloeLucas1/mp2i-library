(** [sum l] returns the sum of elements of [l] *)
let rec sum l = match l with
    | [] -> 0
    | e::q -> e + sum q;;

(** [find x l] search an element x in a list l *)
let rec find x l = match l with
    | [] -> false
    | e::q -> e = x || find x q

(** [reverse l] reverse a list l *)
let reverse l =
    let rec rev l acc = match l with
        | [] -> acc
        | e::q -> rev q (e::acc) in
    rev l []

(** [increasing l] return if the list is increasing or not *)
let rec increasing l = match l with
    | [] -> true
    | [e] -> true
    | e1::e2::q -> e1 <= e2 && increasing (e2::q)
