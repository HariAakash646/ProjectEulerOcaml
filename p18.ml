let rec dp_val row =
  if row > 15 then Array.init row (fun i -> 0)
  else
    let arr =
      Array.of_list
        (List.map int_of_string (String.split_on_char ' ' (read_line ())))
    in
    let out_arr = dp_val (row + 1) in

    let rec update_arr id =
      if id >= row then ()
      else (
        arr.(id) <- arr.(id) + max out_arr.(id) out_arr.(id + 1);
        update_arr (id + 1))
    in
    update_arr 0;
    arr
;;

print_int (dp_val 1).(0)
