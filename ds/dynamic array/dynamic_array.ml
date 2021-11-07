type 'a array_dyn = {mutable t : 'a array; mutable n : int};;

(** [copy t1 t2] copies elements of an array t1 in an array t2 *)
let copy t1 t2 =
  for i = 0 to Array.length t1 - 1 do
    t2.(i) <- t1.(i)
  done;;

(** [add e d] adds an element e to a dynamic array d *)
let add e d =
  if d.n < Array.length d.t then (d.t.(d.n) <- e; d.n <- d.n + 1)
  else if d.n = 0 then (d.t <- [|e|]; d.n <- 1)
  else (let t' = Array.make (2*d.n) 0 in
        copy d.t t';
        t'.(d.n) <- e;
        d.t <- t';
        d.n <- d.n + 1)
