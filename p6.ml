let sum_upto_n n = n * (n + 1) / 2
let sum_squared_upto_n n = n * (n + 1) * ((2 * n) + 1) / 6
let n = 100;;

print_int ((sum_upto_n n * sum_upto_n n) - sum_squared_upto_n n)
