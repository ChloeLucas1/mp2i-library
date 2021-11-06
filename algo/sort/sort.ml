(* This file contains sorting algorithms *)

(** [swap t i j] exchanges [t.(i)] and [t.(j)] *)
let swap t i j =
  let tmp = t.(i) in
  t.(i) <- t.(j);
  t.(j) <- tmp;;

(** [sort_bubble t] sorts array t using bubble sorting (O(n**2)) *)
let sort_bubble t = 
  for _ = 0 to Array.length t - 1 do
    for j = 0 to Array.length t - 2 do
      if t.(j) > t.(j + 1) then swap t j (j + 1)
      done
  done;;

(** [split l] splits a list l into two lists *)
let rec split l = match l with
    | [] -> [], []
    | [e] -> [e], []
    | e1::e2::q -> let l1, l2 = split q in
                   (e1::l1), (e2::l2)

(** [fusion l1 l2] merges two sorted lists into one sorted list *)
let rec fusion l1 l2 = match l1, l2 with
    | [], _ -> l2
    | _, [] -> l1
    | e1::q1, e2::q2 when e1 <= e2 -> e1::(fusion q1 l2)
    | e1::q1, e2::q2 -> e2::(fusion l1 q2)

(** [merge_sort] sorts a list l *)
let rec merge_sort l = match l with
    | [] -> []
    | [e] -> [e]
    | l -> let l1, l2 = split l in
           fusion (merge_sort l1) (merge_sort l2)
