let fst_of_pair (x, _) = x
let snd_of_pair (_, y) = y

let collatz_seq_dp n =
  let dp_hash = Hashtbl.create (2 * n) in

  let rec compute_value x =
    match Hashtbl.find_opt dp_hash x with
    | Some v -> v
    | None ->
        let v =
          if x = 1 then 0
          else if x mod 2 = 0 then compute_value (x / 2)
          else 1 + compute_value ((3 * x) + 1)
        in
        Hashtbl.add dp_hash x v;
        v
  in

  List.fold_left
    (fun acc x -> if compute_value x >= snd_of_pair acc then (x, compute_value x) else acc)
    (0, 0)
    (List.init (n + 1) (fun i -> i+1))
;;

print_int (fst_of_pair (collatz_seq_dp 1000000))
