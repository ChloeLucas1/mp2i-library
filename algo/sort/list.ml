(* This file contains list algorithms *)

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
