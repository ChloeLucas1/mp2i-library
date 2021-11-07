type ('a, 'b) dictionary = {hash: 'a -> int; datas: ('a * 'b) list array; width: int};;

(** [create h w] creates a dictionary of size w with a hash table h *)
let create h w =
    {hash = h; datas = Array.make w []; width = w}

(** [search t k] returns if there is a key k or not in a dictionary t *)
let search t k =
    t.datas.(t.hash k)
    |> List.map fst
    |> List.mem k

(** [element t k] return the value associated with the key k in a dictionnary t, if the key exists *)
let element t k =
    List.filter (fun c -> fst c = k) t.datas.(t.hash k)
    |> List.hd

(** [add t k e] adds an element e with the key k in a dictionary t *)
let add t k e =
    t.datas.(t.hash k) <- (k, e)::t.datas.(t.hash k)

(** [delete t k] deletes the element associated with the key k in a dictionary t, if the key exists *)
let delete t k =
    t.datas.(t.hash k) <- List.filter (fun c -> fst c <> k) t.datas.(t.hash k)
