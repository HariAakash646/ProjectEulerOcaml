let input_and_process_names =
  let names = Array.of_list (List.sort compare (String.split_on_char ',' (read_line ()))) in

  let rec process_name id =
    if id >= Array.length names then 0
    else
    (let name = names.(id) in

    let rec name_val id =
      if Char.equal name.[id] '"' then 0
      else int_of_char name.[id] - int_of_char 'A' + 1 + name_val (id + 1)
    in
    
    (name_val 1) * (id + 1) + process_name (id + 1))
  in

  process_name 0
;;

print_int input_and_process_names