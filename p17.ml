module IntMap = Map.Make (Int)

let int_to_val =
  IntMap.empty |> IntMap.add 1 "one" |> IntMap.add 2 "two"
  |> IntMap.add 3 "three" |> IntMap.add 4 "four" |> IntMap.add 5 "five"
  |> IntMap.add 6 "six" |> IntMap.add 7 "seven" |> IntMap.add 8 "eight"
  |> IntMap.add 9 "nine" |> IntMap.add 10 "ten" |> IntMap.add 11 "eleven"
  |> IntMap.add 12 "twelve" |> IntMap.add 13 "thirteen"
  |> IntMap.add 14 "fourteen" |> IntMap.add 15 "fifteen"
  |> IntMap.add 16 "sixteen" |> IntMap.add 17 "seventeen"
  |> IntMap.add 18 "eighteen" |> IntMap.add 19 "nineteen"
  |> IntMap.add 20 "twenty" |> IntMap.add 30 "thirty" |> IntMap.add 40 "forty"
  |> IntMap.add 50 "fifty" |> IntMap.add 60 "sixty" |> IntMap.add 70 "seventy"
  |> IntMap.add 80 "eighty" |> IntMap.add 90 "ninety"

let rec int_name_length n =
  if n >= 100 then
    String.length (IntMap.find (n / 100) int_to_val)
    +
    if n mod 100 > 0 then
      String.length "hundredand" + int_name_length (n mod 100)
    else String.length "hundred"
  else if n >= 20 then
    String.length (IntMap.find (n / 10 * 10) int_to_val)
    + int_name_length (n mod 10)
  else if n > 0 then String.length (IntMap.find n int_to_val)
  else 0

let rec total_length n =
  if n <= 0 then 0 else int_name_length n + total_length (n - 1)
;;

print_int (total_length 999 + String.length "onethousand")
