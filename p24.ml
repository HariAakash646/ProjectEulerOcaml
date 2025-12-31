let rec reverse array l r =
  if l >= r then ()
  else
    let tmp = array.(l) in
    array.(l) <- array.(r);
    array.(r) <- tmp;
    reverse array (l + 1) (r - 1)

let rec print_array array id =
  if id >= Array.length array then ()
  else (
    print_int array.(id);
    print_array array (id + 1))

let next_permutation array =
  let n = Array.length array in
  let rec find_lower id x =
    if id < 0 then id
    else if array.(id) < x then id
    else find_lower (id - 1) array.(id)
  in
  let id = find_lower (n - 2) array.(n - 1) in

  if id < 0 then (
    print_array array 0;
    failwith "-1 spotted");

  let rec find_greater id x =
    if array.(id) > x then id else find_greater (id - 1) x
  in

  let id2 = find_greater (n - 1) array.(id) in
  let tmp = array.(id2) in
  array.(id2) <- array.(id);
  array.(id) <- tmp;
  reverse array (id + 1) (n - 1)

let rec loop_perms array n =
  if n <= 0 then ()
  else (
    next_permutation array;
    loop_perms array (n - 1))

let array = Array.init 10 (fun i -> i);;

loop_perms array (1000000 - 1);;
print_array array 0
