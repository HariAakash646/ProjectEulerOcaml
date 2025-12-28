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

let sum_prime_upto n =
  let is_prime = sieve n in

  List.fold_left
    (fun acc x -> if is_prime.(x) then acc + x else acc)
    0
    (List.init (n + 1) (fun i -> i))
;;

print_int (sum_prime_upto 2000000)
