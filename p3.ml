let rec modexp (x : int) (r : int) (m : int) =
  if r == 0 then 1
  else if r < 0 then failwith "Exponent mustbe a positive number"
  else
    let v = modexp x (r / 2) m in
    match r mod 2 with 0 -> v * v mod m | _ -> v * v * x mod m

let miller_rabin_primality_test (p : int) =
  match p mod 2 with
  | 0 -> ( match p with 2 -> true | _ -> false)
  | _ -> (
      let a = 2 + Random.int 100000007 in
      let d = p - 1 in
      match modexp a d p with
      | 1 ->
          let rec check_exps a d =
            match modexp a d p with
            | x when x = p - 1 -> true
            | 1 -> ( match d mod 2 with 1 -> true | _ -> check_exps a (d / 2))
            | _ -> false
          in
          check_exps a (d / 2)
      | _ -> false)

let rec test_prime n x =
  match x with
  | 0 -> true
  | _ -> (
      match miller_rabin_primality_test n with
      | false -> false
      | true -> test_prime n (x - 1))

let sqrt_of_int (x : int) = int_of_float (sqrt (float_of_int x))

let largest_prime_factor (n : int) =
  let rec search_for_primes x =
    match x * x > n with
    | true -> -1
    | false -> (
        match n mod x with
        | 0 -> (
            let quotient = n / x in
            match test_prime quotient 8 with
            | true -> quotient
            | false -> (
                match test_prime x 8 with
                | true -> max x (search_for_primes (x + 1))
                | false -> search_for_primes (x + 1)))
        | _ -> search_for_primes (x + 1))
  in

  search_for_primes 2
;;

print_int (largest_prime_factor 600851475143)
