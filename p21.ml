let rec proper_divisor_sum x curr =
  if curr * curr > x then 0
  else if x mod curr = 0 then
    proper_divisor_sum x (curr + 1)
    + if curr * curr = x then curr else curr + (x / curr)
  else proper_divisor_sum x (curr + 1)

let rec sum_amicable_numbers n =
  if n <= 1 then 0
  else
    let v1 = (if n > 1 then 1 else 0) + proper_divisor_sum n 2 in
    let v2 = (if n > 1 then 1 else 0) + proper_divisor_sum v1 2 in

    (if v2 = n && v1 != v2 then n else 0) + sum_amicable_numbers (n - 1)
;;

print_int (sum_amicable_numbers 10000)
(* print_int (proper_divisor_sum 284 2 + 1) *)
