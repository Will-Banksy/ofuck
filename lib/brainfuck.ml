type bf_tok = None | Forwards | Backwards | Increment | Decrement | Output | Input | JumpForwardsIfZero | JumpBackwardsIfnZero ;;

exception InvalidCommand

exception Todo

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

let eval_tok tok tape ptr in_chan out_chan = match tok with
  | None -> (tape, ptr)
  | Forwards -> (tape, ptr + 1)
  | Backwards -> (tape, Int.max 0 (ptr - 1))
  | Increment -> (
    tape |> List.mapi (fun i n -> if i = ptr then n + 1 else n),
    ptr
  )
  | Decrement -> (
    tape |> List.mapi (fun i n -> if i = ptr then n + 1 else n),
    ptr
  )
  | Output -> (
    tape |> List.mapi (fun i n -> if i = ptr then (n |> Char.chr |> output_char out_chan; n) else n),
    ptr
  )
  | Input -> (
    tape |> List.mapi (fun i n -> if i = ptr then (input_char in_chan) |> Char.code else n),
    ptr
  )
  (* TODO: Finish *)
  | _ -> raise Todo