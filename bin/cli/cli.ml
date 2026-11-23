module Vm = Uxn.Make (Varvara.Console)

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

let run_vm m program args =
  Vm.load_program m program;
  Vm.write_to_port m ~port:0x17 ~value:(if List.length args > 1 then 1 else 0);
  let run_until_break () =
    let rec loop () =
      match Vm.step m with
      | Vm.Continue -> loop ()
      | Vm.Break -> Vm.check_halt m
    in
    loop ()
  in
  run_until_break ()
;;

let handle_run_cmd argv =
  let rom = ref "" in
  let speclist =
    [ ("--rom", Arg.Set_string rom, "<path> Path of the ROM file") ] |> Arg.align
  in
  (* collect all the additional arguments passed along with the command *)
  let stdin_args = ref [] in
  let collect_args arg = stdin_args := arg :: !stdin_args in
  Arg.parse_argv argv speclist collect_args usage_msg;
  if !rom = "" then raise @@ Arg.Bad "missing required argument: --rom";
  let program = read_rom_file !rom in
  let machine = Vm.make () in
  Vm.load_program machine program;
  run_vm machine program !stdin_args
;;

let () =
  match Array.length Sys.argv with
  | 0 | 1 ->
    prerr_endline usage_msg;
    exit 2
  | _ ->
    ( try handle_run_cmd Sys.argv with
      | Arg.Bad msg ->
        prerr_endline msg;
        exit 2
      | Arg.Help msg -> print_string msg
    )
;;
