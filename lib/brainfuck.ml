type bf_tok = None | Forwards | Backwards | Increment | Decrement | Output | Input | JumpForwardsIfZero | JumpBackwardsIfnZero ;;

exception InvalidCommand

exception NoMatchingBracket

let to_tok b = match b with
  | '>' -> Forwards
  | '<' -> Backwards
  | '+' -> Increment
  | '-' -> Decrement
  | '.' -> Output
  | ',' -> Input
  | '[' -> JumpForwardsIfZero
  | ']' -> JumpBackwardsIfnZero
  | '\n' | '\t' | ' ' | '\r' -> None
  | _ -> raise InvalidCommand

let parse_bytes b = b |> String.to_seq |> List.of_seq |> List.map to_tok |> List.filter (fun t -> t != None)

let eval_tok toks tape ptr in_chan out_chan = match ptr |> List.nth toks with
  | None -> (tape, ptr)
  | Forwards -> (
    (if (tape |> List.length) <= (ptr + 1) then tape @ [0] else tape),
    ptr + 1
  )
  | Backwards -> (tape, Int.max 0 (ptr - 1))
  | Increment -> (
    tape |> List.mapi (fun i n -> if i = ptr then n + 1 else n),
    ptr + 1
  )
  | Decrement -> (
    tape |> List.mapi (fun i n -> if i = ptr then n + 1 else n),
    ptr + 1
  )
  | Output -> (
    tape |> List.mapi (fun i n -> if i = ptr then (n |> Char.chr |> output_char out_chan; n) else n),
    ptr + 1
  )
  | Input -> (
    tape |> List.mapi (fun i n -> if i = ptr then (input_char in_chan) |> Char.code else n),
    ptr + 1
  )
  | JumpForwardsIfZero -> (
    tape,
    match toks |> List.find_mapi (fun i t -> if i > ptr && t = JumpBackwardsIfnZero then Some (i + 1) else None) with
      | Some i -> i
      | None -> raise NoMatchingBracket
  )
  | JumpBackwardsIfnZero -> (
    tape,
    match toks |> List.mapi (fun i t -> (i, t)) |> List.rev |> List.find_map (fun (i, t) -> if i > ptr && t = JumpBackwardsIfnZero then Some (i + 1) else None) with
      | Some i -> i
      | None -> raise NoMatchingBracket
  )