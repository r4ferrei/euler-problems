let rec solve curr = match curr with
    | 0 -> 0
    | _ when (curr mod 3 = 0) || (curr mod 5 = 0) -> curr + (solve (curr - 1))
    | _ -> solve (curr - 1);;

let rec solve2 curr = match curr with
    | 0 -> 0
    | _ -> (if (curr mod 3 = 0) || (curr mod 5 = 0) then curr else 0) +
    solve2 (curr - 1);;

let rec range a b = match a with
    | _ when a > b -> []
    | _ -> a :: (range (a + 1) b);;

let solve3 curr =
    List.fold_left (fun acc x -> acc + x) 0
    (List.filter (fun x -> x mod 3 = 0 || x mod 5 = 0) (range 1 curr));;

Printf.printf "%d\n" (solve 999);
Printf.printf "%d\n" (solve2 999);
Printf.printf "%d\n" (solve3 999)
