let rec big_fact x =
  if Z.equal x (Z.of_int 0) then Z.of_int 1
  else Z.(x * big_fact (x - Z.of_int 1))

let rec sum_digits num =
  match num with
  | head :: suffix -> int_of_char head - int_of_char '0' + sum_digits suffix
  | _ -> 0
;;

print_int
  (sum_digits
     (List.of_seq (String.to_seq (Z.to_string (big_fact (Z.of_int 100))))))
