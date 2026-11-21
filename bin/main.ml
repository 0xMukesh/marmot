let usage_msg = "usage: marmot --rom <path>"

let read_rom_file rom =
  let program = Array.make 0xff00 0 in
  let ic = In_channel.open_bin rom in
  Array.iteri
    (fun i _ ->
       match In_channel.input_byte ic with
       | Some c -> program.(i) <- c
       | None -> ()
     )
    program;
  In_channel.close ic;
  program
;;

let run argv =
  let rom = ref "" in
  let speclist =
    [ ("--rom", Arg.Set_string rom, "<path> Path of the ROM file") ] |> Arg.align
  in
  (* skip `run`*)
  let current = ref 1 in
  (* collect all the additional arguments passed along with the command *)
  let stdin_args = ref [] in
  let collect_args arg = stdin_args := arg :: !stdin_args in
  Arg.parse_argv ~current argv speclist collect_args usage_msg;
  if !rom = "" then raise @@ Arg.Bad "missing required argument: --rom";
  let program = read_rom_file !rom in
  let machine = Uxn.make program in
  Uxn.run machine (List.rev !stdin_args)
;;

let () =
  match Array.length Sys.argv with
  | 0 | 1 ->
    prerr_endline usage_msg;
    exit 2
  | _ ->
    ( try run Sys.argv with
      | Arg.Bad msg ->
        prerr_endline msg;
        exit 2
      | Arg.Help msg -> print_string msg
    )
;;
