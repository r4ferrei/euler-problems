let expandFib fibs = match fibs with
    | []
    | [_] -> [0; 1]
    | a::b::_ -> (a + b) :: fibs;;

let rec getFibs fibs = match (expandFib fibs) with
    | a::_ as ans when a >= 4000000 -> ans
    | ans -> getFibs ans;;

let ans =
    List.fold_left (fun acc x -> acc + x) 0
    (List.filter (fun x -> x mod 2 = 0) (getFibs []));;

Printf.printf "%d\n" ans
