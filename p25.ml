let thousand_dig = Z.pow (Z.of_int 10) 999

let rec process_fibs a b id =
  if b >= thousand_dig then id else process_fibs b Z.(a + b) (id + 1)
;;

print_int (process_fibs (Z.of_int 1) (Z.of_int 1) 2)
