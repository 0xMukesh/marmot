module Console = Varvara.Console
module Vm = Uxn.Make (Console)

let run ~path ~args =
  let program = Helpers.read_rom_file ~path in
  let m = Vm.make () in
  Vm.load_program m program;

  let device = Vm.device m in
  Vm.load_program m program;

  let console_type_port = 0x17 in
  Vm.write_port
    m
    ~port:console_type_port
    ~value:(if List.length args > 1 then 1 else 0)
    ~short:false;

  let run_until_break () =
    let rec loop () =
      match Vm.step m with
      | Vm.Continue -> loop ()
      | Vm.Break -> Vm.check_halt m
    in
    loop ()
  in
  let read_console_vector () = Vm.read_port m ~port:0x10 ~short:true in
  let feed_arg arg ~final =
    String.iter
      (fun c -> Console.push device ~data:(Char.code c) ~kind:Console.Argument)
      arg;
    Console.push
      device
      ~data:(Char.code '\n')
      ~kind:(if final then Console.End else Console.ArgumentSpacer);

    let ports = Vm.ports m in
    while Console.prepare device ~ports do
      Vm.set_pc m (read_console_vector ());
      run_until_break ()
    done
  in
  let rec process_args args =
    match args with
    | hd :: tl ->
      feed_arg hd ~final:(tl = []);
      process_args tl
    | [] -> ()
  in
  run_until_break ();
  if read_console_vector () <> 0 then process_args args
;;
