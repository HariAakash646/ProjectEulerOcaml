let rec gcd a b =
  match b with 0 -> a | _ -> gcd b (a mod b)

let rec lcm_upto_x x =
  match x with 
  | 1 -> 1
  | _ -> 
    (let l = lcm_upto_x (x - 1) in
    l * x / (gcd l x))
;;

print_int (lcm_upto_x 20)