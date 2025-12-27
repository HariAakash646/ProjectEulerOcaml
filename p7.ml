let sieve n =
  let is_prime = Array.make (n + 1) true in
  is_prime.(0) <- false;
  is_prime.(1) <- false;

  let rec mark_false x v =
    if v <= n then (
      is_prime.(v) <- false;
      mark_false x (v + x))
  in

  let rec loop_elem x =
    if x <= n then (
      if is_prime.(x) then mark_false x (x * x);
      loop_elem (x + 1))
  in

  loop_elem 2;

  is_prime

let is_prime = sieve 400000

let rec find_xth_prime x id =
  if is_prime.(id) then
    match x with 1 -> id | _ -> find_xth_prime (x - 1) (id + 1)
  else find_xth_prime x (id + 1)
;;

print_int (find_xth_prime 10001 0)
