let rec find_index f list id =
  match list with
  | [] -> None
  | hd :: suffix -> if f hd then Some id else find_index f suffix (id + 1)

let rec find_recur prev_val dividend divisor =
  let q = dividend / divisor in
  let r = dividend mod divisor in

  if r = 0 then 0
  else
    match find_index (fun t -> t = (q, r)) prev_val 0 with
    | None -> find_recur ([ (q, r) ] @ prev_val) (r * 10) divisor
    | Some id -> id + 1

let rec find_longest_recurring x =
  print_int x;
  print_newline ();
  if x <= 0 then 0 else max (find_recur [] 1 x) (find_longest_recurring (x - 1))

let snd_of_pair (_, y) = y
let fst_of_pair (x, _) = x;;

print_int
  (fst_of_pair
     (List.fold_left
        (fun acc x ->
          let v = find_recur [] 1 x in
          if v > snd_of_pair acc then (x, v) else acc)
        (0, 0)
        (List.init 1000 (fun i -> i + 1))))
