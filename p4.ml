let reverse_string s =
  let len = String.length s in
  String.init len (fun i -> String.get s (len - 1 - i))

let is_palindrome x =
  let x_str = string_of_int x in
  String.equal x_str (reverse_string x_str)

let rec find_max_3x3_palindrome x y =
  let x, y = match y >= 1000 with true -> (x + 1, 100) | false -> (x, y) in

  match x >= 1000 with
  | true -> 0
  | false -> (
      let prod = x * y in
      match is_palindrome prod with
      | false -> find_max_3x3_palindrome x (y + 1)
      | true -> max prod (find_max_3x3_palindrome x (y + 1)))
;;

print_int (find_max_3x3_palindrome 100 100)
