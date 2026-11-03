let usage_msg = "usage: marmot --rom <path>"

(* helpers *)
let read_rom_file rom =
  let program = Array.init 0xff00 (fun _ -> 0) in
  let i = ref 0 in
  In_channel.with_open_bin rom (fun ic ->
    let rec copy () =
      match In_channel.input_byte ic with
      | Some c ->
        program.(!i) <- c;
        i := !i + 1;
        copy ()
      | None -> ()
    in
    copy ()
  );
  program
;;

(* core *)
let run argv =
  (* let rom = ref "" in
  let speclist =
    [ ("--rom", Arg.Set_string rom, "<path> Path of the ROM file") ] |> Arg.align
  in
  let handle_unknown_arg arg = raise @@ Arg.Bad ("unexpected argument: " ^ arg) in
  Arg.parse_argv argv speclist handle_unknown_arg usage_msg;
  if !rom = "" then raise @@ Arg.Bad "missing required argument: --rom";
  let program = read_rom_file !rom in
  let machine = Uxn.init_machine program in *)
  ()
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
