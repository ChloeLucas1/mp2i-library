(** [max_slice t] returns the maximum value of a consecutive sum *)
let max_slice t =
    let maxi = ref t.(0) in
    let n = Array.length t in
    for i = 0 to n - 1 do
        let sum = ref 0 in
        for j = i to n - 1 do
            sum := !sum + t.(j); (* sum contient la somme de t.(i) à t.(j) *)
            maxi := max !maxi !sum
        done;
    done;
    !maxi
