let rec fact x = match x with 0 -> 1 | _ -> x * fact (x - 1)
let combi n r = if r > n then 0 else fact n / (fact r * fact (n - r));;

(* I decided to just list out the numbers to prevent overflow since that seemed to be faster than actually coding up aclever way to do this. *)
print_int
  (40 / (20 * 2)
  * (39 / (3 * 13))
  * 38 / 19 * 37
  * (36 / (9 * 4))
  * 35 / (7 * 5) * 34 / 17 * 33 / 11 * 32 / 16 / 8 * 31 * 30 / 10 * 29 * 28 / 14
  / 6 * 27 * 26 / 18 * 25 * 24 / 12 * 23 * 22 * 21 / 15)
