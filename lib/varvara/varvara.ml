module System : Uxn.Device = System
module Console : Device_intf.Console = Console

module Varvara = struct
  type t =
    { system : System.t
    ; console : Console.t
    }

  let make ~ram = { system = System.make ~ram; console = Console.make ~ram }

  let read t ~port ~current =
    match port land 0xf0 with
    | 0x00 -> System.read t.system ~port ~current
    | 0x10 -> Console.read t.console ~port ~current
    | _ -> current
  ;;

  let write t ~port ~value =
    match port land 0xf0 with
    | 0x00 -> System.write t.system ~port ~value
    | 0x10 -> Console.write t.console ~port ~value
    | _ -> ()
  ;;
end
