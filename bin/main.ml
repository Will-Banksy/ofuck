open Printf
open Ofuck

let get_lines () = let lines_mut = ref [] in
  try
    while true do
      lines_mut := !lines_mut @ [read_line ()]
    done
  with End_of_file -> !lines_mut ;;

let () = let toks = get_lines () |> List.map (Brainfuck.parse_bytes) |> List.concat |> List.filter (fun e -> e != Brainfuck.NoTok) in
  printf "%s\n%!" "Starting brainfuck interpreter...";
  (* List.length toks |> printf "Num tokens: %i\n"; *)
  Brainfuck.eval_toks toks [0] 0 0 stdin stdout ;;
