let x = Z.of_int 2
let p = Z.of_int 1000

let num =
  List.map
    (fun x -> int_of_char x - int_of_char '0')
    (List.of_seq (String.to_seq (Z.to_string (Z.pow x 1000))))
;;

print_int (List.fold_left (fun acc x -> acc + x) 0 num)

(* Compile with `ocamlfind ocamlopt -linkpkg -package zarith p16.ml -o out` *)
