let rec divisor_count x curr =
  if curr * curr > x then 0
  else if x mod curr = 0 then
    divisor_count x (curr + 1) + if curr * curr = x then 1 else 2
  else divisor_count x (curr + 1)

let rec triangle_num_with_500_divisors n =
  if divisor_count (n * (n + 1) / 2) 1 >= 500 then n * (n + 1) / 2
  else triangle_num_with_500_divisors (n + 1)
;;

print_int (triangle_num_with_500_divisors 1)