type control_flow = Continue | Stop

let enumerate l = l |> List.mapi (fun i e -> (i, e))

let rec fold_while f a l = match f a (l |> List.hd) with
  | (acc, Continue) -> if l |> List.tl |> List.length == 0 then
    acc
  else
    fold_while f acc (l |> List.tl)
  | (acc, Stop) -> acc