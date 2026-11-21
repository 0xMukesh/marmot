module System : Device_intf.Device = System
module Console : Device_intf.Console = Console

type t =
  { ports : Memory.t
  ; system : System.t
  ; console : Console.t
  }

let make ~ram =
  { ports = Memory.make ~n:0x100
  ; system = System.create ~ram
  ; console = Console.create ~ram
  }
;;

let console_vector t = Memory.read_short t.ports 0x10
let push_input t ~data ~kind = Console.push t.console ~data ~kind
let prepare_input t = Console.prepare t.console ~ports:t.ports

let read t ~port ~short =
  let current = Memory.read t.ports port ~short in
  match port land 0xf0 with
  | 0x00 -> System.read t.system ~port ~current
  | 0x10 -> Console.read t.console ~port ~current
  | _ -> current
;;

let write t ~port ~value ~short =
  Memory.write t.ports port ~value ~short;
  match port land 0xf0 with
  | 0x00 -> System.write t.system ~port ~value
  | 0x10 -> Console.write t.console ~port ~value
  | _ -> ()
;;
