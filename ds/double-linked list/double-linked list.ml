type 'a 2l_list = { elem : 'a; mutable prev : 'a 2l_list; mutable next : 'a 2l_list };;

(** [create e] creates a 2l_list with an element e *)
let create e =
    let rec l = { elem = e; prev = l; next = l } in l;;

(** [add l e] adds 2l_list with an element e to a 2l_list l *)
let add l e = 
    let l_new = { elem = e; prev = l; next = l.next } in
    l.next.prev <- l_new;
    l.next <- l_new;;

(** [delete l] deletes a 2l_list l *)
let delete l =
    l.prev.next <- l.next;
    l.next.prev <- l.prev;;

(** [length l] return the total size of the 2l_list l *)
let length l =
    let rec aux l1 =
        if l1 == l then 1
        else 1 + aux l1.next in
    aux l.next;;

(** [mem l e] returns if an element e is or not in the 2l_list l *)
let mem l e =
    let cur = ref l.next in
    while !cur.elem <> e && !cur != l do
        cur := !cur.next
    done;
    !cur.elem = e;;

(** [fusion l1 l2] merges two 2l_lists l1 and l2 *)
let fusion l1 l2 =
    l1.next.prev <- l2;
    l2.next.prev <- l1;
    let l1n = l1.next in
    l1.next <- l2.next;
    l2.next <- l1n
