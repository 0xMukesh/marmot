module Console = Varvara.Console
module Vm = Uxn.Make (Console)

let run_command =
  Command.basic
    ~summary:"Run a Uxn program"
    (let open Command.Let_syntax in
     let%map_open rom = flag "rom" (required string) ~doc:"FILE path of the ROM file"
     and args = anon (sequence ("ARG" %: string)) in
     fun () -> Commands.run ~path:rom ~args
    )
;;

let command = Command.group ~summary:"Marmot :: A Uxn Emulator" [ ("run", run_command) ]
let () = Command_unix.run command
