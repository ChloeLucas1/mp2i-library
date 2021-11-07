type 'a queue = {l1 : 'a list; l2 : 'a list};;

(** [queue_empty f] returns if a queue f is empty or not *)
let queue_empty f = f.l1 = [] && f.l2 = [];;

(** [queue_add f e] adds an element e to a queue f *)
let queue_add f e = {l1 = e::f.l1; l2 = f.l2};;

(** [queue_pop f] deletes and returns the first element of a queue f *)
let rec queue_pop f = match f.l1 with
| e::q -> e, {l1 = q; l2 = f.l2}
| [] -> queue_pop {l1 = List.rev f.l2; l2 = []};;
