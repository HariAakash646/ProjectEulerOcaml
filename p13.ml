let num_list = Array.init 50 (fun i -> Array.init 100 (fun i -> 0))

let rec input_num id =
  if id >= 100 then ()
  else
    let num =
      Array.of_list
        (List.rev
           (List.map
              (fun x -> int_of_char x - int_of_char '0')
              (List.of_seq (String.to_seq (read_line ())))))
    in

    let rec populate_array dig =
      if dig >= 50 then ()
      else (
        num_list.(dig).(id) <- num.(dig);
        populate_array (dig + 1))
    in
    populate_array 0;
    input_num (id + 1)
;;

input_num 0

let split_num x =
  let num = string_of_int x in
  if String.length num > 1 then
    ( int_of_string (String.sub num (String.length num - 1) 1),
      int_of_string (String.sub num 0 (String.length num - 1)) )
  else (x, 0)

let rec compute_sum id x =
  if id >= 50 then [ x ]
  else
    let sum = Array.fold_left (fun acc x -> acc + x) x num_list.(id) in

    let ones, rest = split_num sum in
    print_int ones;
    print_string " ";
    print_int rest;
    print_endline "";
    [ ones ] @ compute_sum (id + 1) rest

let sum_list = List.rev (compute_sum 0 0)

let rec print_list id sum_list =
  if id >= 10 then ()
  else
    match sum_list with
    | head :: suffix ->
        print_int head;
        print_list (id + 1) suffix
    | _ -> ()
;;

print_list 0 sum_list
