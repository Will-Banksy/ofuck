type bf_tok = NoTok | Forwards | Backwards | Increment | Decrement | Output | Input | JumpForwardsIfZero | JumpBackwardsIfnZero ;;

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
  | '\n' | '\t' | ' ' | '\r' -> NoTok
  | _ -> raise InvalidCommand

let print_tok out_chan t = output_char out_chan (match t with
  | Forwards -> '>'
  | Backwards -> '<'
  | Increment -> '+'
  | Decrement -> '-'
  | Output -> '.'
  | Input -> ','
  | JumpForwardsIfZero -> '['
  | JumpBackwardsIfnZero -> ']'
  | NoTok -> 'N')

let parse_bytes b = b |> String.to_seq |> List.of_seq |> List.map to_tok |> List.filter (fun t -> t != NoTok)

let find_matching_jbnz toks ip = let (_, matching) =
  toks |> ListExt.enumerate
  (* Filter out elements before ip *)
  |> List.filter (fun (i, _) -> if i > ip then true else false)
  (* Loop through rest of list, incrementing accumulator on [ and decrementing on ] until zero *)
  |> ListExt.fold_while (fun (depth, _) (i, e) ->
      if e == JumpForwardsIfZero then
        ((depth + 1, None), ListExt.Continue)
      else if e == JumpBackwardsIfnZero then
        if depth == 0 then
          ((depth, Some(i)), ListExt.Stop)
        else
          ((depth - 1, None), ListExt.Continue)
      else
        ((depth, None), ListExt.Continue)
    ) (0, None) in
  matching

let find_matching_jfz toks ip = let (_, matching) =
  toks |> ListExt.enumerate
  |> List.filter (fun (i, _) -> if i < ip then true else false)
  |> List.rev
  |> ListExt.fold_while (fun (depth, _) (i, e) ->
    if e == JumpBackwardsIfnZero then
      ((depth + 1, None), ListExt.Continue)
    else if e == JumpForwardsIfZero then
      if depth == 0 then
        ((depth, Some(i)), ListExt.Stop)
      else
        ((depth - 1, None), ListExt.Continue)
    else
      ((depth, None), ListExt.Continue)
    ) (0, None) in
  matching

let eval_tok toks tape inst_ptr dat_ptr in_chan out_chan = match inst_ptr |> List.nth toks with
  | NoTok -> (tape, inst_ptr + 1, dat_ptr)
  | Forwards -> (
    (if (tape |> List.length) <= (dat_ptr + 1) then tape @ [0; 0] else tape),
    inst_ptr + 1,
    dat_ptr + 1
  )
  | Backwards -> (
    tape,
    inst_ptr + 1,
    Int.max 0 (dat_ptr - 1))
  | Increment -> (
    tape |> List.mapi (fun i n -> if i = dat_ptr then n + 1 else n),
    inst_ptr + 1,
    dat_ptr
  )
  | Decrement -> (
    tape |> List.mapi (fun i n -> if i = dat_ptr then n - 1 else n),
    inst_ptr + 1,
    dat_ptr
  )
  | Output -> (
    tape |> List.mapi (fun i n -> if i = dat_ptr then (n |> Char.chr |> output_char out_chan; n) else n),
    inst_ptr + 1,
    dat_ptr
  )
  | Input -> (
    tape |> List.mapi (fun i n -> if i = dat_ptr then (input_char in_chan) |> Char.code else n),
    inst_ptr + 1,
    dat_ptr
  )
  | JumpForwardsIfZero -> (
    tape,
    (if try (List.nth tape dat_ptr) != 0 with Failure _ -> assert false then inst_ptr + 1 else match find_matching_jbnz toks inst_ptr with
      | Some i -> i
      | None -> raise NoMatchingBracket),
    dat_ptr
  )
  | JumpBackwardsIfnZero -> (
    tape,
    (if (List.nth tape dat_ptr) == 0 then inst_ptr + 1 else match find_matching_jfz toks inst_ptr with
      | Some i -> i
      | None -> assert false),
    dat_ptr
  )

let rec eval_toks toks tape inst_ptr dat_ptr in_chan out_chan =
  (* Printf.printf "IP: %i (%a), DAT: %i, *DAT: %i\n%!" inst_ptr print_tok (List.nth toks inst_ptr) dat_ptr (List.nth tape dat_ptr); *)
  (* Thread.delay 0.1; *)
  let (next_tape, next_inst_ptr, next_dat_ptr) = eval_tok toks tape inst_ptr dat_ptr in_chan out_chan in
  if next_inst_ptr >= (toks |> List.length) then
    ()
  else
    eval_toks toks next_tape next_inst_ptr next_dat_ptr in_chan out_chan;;