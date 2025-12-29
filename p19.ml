let month_days = [ 31; 28; 31; 30; 31; 30; 31; 31; 30; 31; 30; 31 ]

let is_leap year =
  if year mod 400 = 0 then true
  else if year mod 100 = 0 then false
  else if year mod 4 = 0 then true
  else false

let rec count_sundays year month day =
  let year, month = if month >= 12 then (year + 1, 0) else (year, month) in

  let prev_day = day in

  if year > 2000 then 0
  else
    let day = day + List.nth month_days month in
    let day = if month = 1 && is_leap year then day + 1 else day in
    let day = day mod 7 in

    (if prev_day = 0 then 1 else 0) + count_sundays year (month + 1) day
;;

print_int (count_sundays 1901 0 2)
