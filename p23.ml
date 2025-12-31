let n = 28123

let rec divisor_sum x curr =
  if curr * curr > x then 0
  else if x mod curr = 0 then
    divisor_sum x (curr + 1)
    + if curr * curr = x then curr else curr + (x / curr)
  else divisor_sum x (curr + 1)

let rec abundant_numbers x =
  if x > n then []
  else (if divisor_sum x 2 + 1 > x then [ x ] else []) @ abundant_numbers (x + 1)

let abundant_num_list = Array.of_list (abundant_numbers 2)
let sum_of_abundant = Array.init (n + 1) (fun i -> false)

let rec mark_sum_abundant i j =
  let i, j =
    if j >= Array.length abundant_num_list then (i + 1, i + 1) else (i, j)
  in
  if i >= Array.length abundant_num_list then ()
  else
    let x = abundant_num_list.(i) + abundant_num_list.(j) in
    if x <= n then sum_of_abundant.(x) <- true;
    mark_sum_abundant i (j + 1)
;;

mark_sum_abundant 0 0

let rec count_non_sum_abundant id =
  if id > n then 0
  else
    (if not sum_of_abundant.(id) then id else 0)
    + count_non_sum_abundant (id + 1)
;;

print_int (count_non_sum_abundant 0)
