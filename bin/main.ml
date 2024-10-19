open Printf
open Ofuck

let rec get_lines_chan in_chan acc =
  match in_chan |> In_channel.input_line with
    | Some line -> get_lines_chan in_chan acc @ [line]
    | None -> acc

let run_bf_chan in_chan out_chan =
  let toks =
    get_lines_chan in_chan []
    |> List.map (Brainfuck.parse_bytes)
    |> List.concat
    |> List.filter (fun e -> e != Brainfuck.NoTok)
  in
    Brainfuck.eval_toks toks [0 |> Char.chr] 0 0 in_chan out_chan

let () = let in_chan =
  if Sys.argv |> Array.length > 1 then
    try
      open_in Sys.argv.(1)
    with Sys_error e -> eprintf "%s\n%!" e; exit 1
  else
    stdin
  in
  eprintf "%s\n%!" "Starting brainfuck interpreter...";
  run_bf_chan in_chan stdout