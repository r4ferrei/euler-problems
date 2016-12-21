let rec is_prime_rec n d =
    if d == n then
        true
    else if n mod d == 0 then
        false
    else if d*d >= n then
        true
    else
        is_prime_rec n (d + 1);;

let is_prime n =
    if n <= 1 then
        false
    else
        is_prime_rec n 2;;

let rec max_prime_factor n d =
    if d*d > n then
        -1
    else if not (is_prime d) then
        max_prime_factor n (d + 1)
    else if n mod d == 0 then (
        if is_prime (n / d)
        then max (n / d) (max_prime_factor n (d + 1))
        else max d (max_prime_factor n (d + 1))
    )
    else
        max_prime_factor n (d + 1);;

Printf.printf "%d\n" (max_prime_factor 13195 2);;
Printf.printf "%d\n" (max_prime_factor 600851475143 2);;
