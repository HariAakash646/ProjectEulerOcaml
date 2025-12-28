let rec input_int_grid n =
  if n > 0 then
    [
      Array.of_list
        (List.map
           (fun x -> int_of_string x)
           (String.split_on_char ' ' (read_line ())));
    ]
    @ input_int_grid (n - 1)
  else []

let grid = Array.of_list (input_int_grid 20)

let rec compute_max_pod i j =
  let n = Array.length grid in
  let m = Array.length grid.(0) in

  let i, j = if j >= m then (i + 1, 0) else (i, j) in
  if i >= Array.length grid then 0
  else
    let max_val = compute_max_pod i (j + 1) in
    let max_val =
      max max_val
        (if i + 3 < n then
           grid.(i).(j) * grid.(i + 1).(j) * grid.(i + 2).(j) * grid.(i + 3).(j)
         else 0)
    in
    let max_val =
      max max_val
        (if j + 3 < m then
           grid.(i).(j) * grid.(i).(j + 1) * grid.(i).(j + 2) * grid.(i).(j + 3)
         else 0)
    in
    let max_val =
      max max_val
        (if i + 3 < n && j + 3 < m then
           grid.(i).(j)
           * grid.(i + 1).(j + 1)
           * grid.(i + 2).(j + 2)
           * grid.(i + 3).(j + 3)
         else 0)
    in

    max max_val
      (if i - 3 >= 0 && j + 3 < m then
         grid.(i).(j)
         * grid.(i - 1).(j + 1)
         * grid.(i - 2).(j + 2)
         * grid.(i - 3).(j + 3)
       else 0)
;;

print_int (compute_max_pod 0 0)
