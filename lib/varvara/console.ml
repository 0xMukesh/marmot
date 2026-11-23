type input_type =
  | Stdin [@value 0x01]
  | Argument
  | ArgumentSpacer
  | End
[@@deriving enum]

type event =
  { data : int
  ; kind : input_type
  }

type t = { pending : event Queue.t }

let make ~ram:_ = { pending = Queue.create () }
let read _ ~port:_ ~current = current

let write t ~port ~value =
  match port with
  | 0x18 ->
    (* Console/write *)
    print_char (Char.chr value);
    flush stdout
  | 0x19 ->
    (* Console/error *)
    prerr_char (Char.chr value);
    flush stderr
  | _ -> ()
;;

let push t ~data ~kind = Queue.add { data; kind } t.pending

let prepare t ~ports =
  if Queue.is_empty t.pending
  then false
  else (
    let event = Queue.take t.pending in
    Memory.write_byte ports ~at:0x12 ~value:event.data;
    Memory.write_byte ports ~at:0x17 ~value:(input_type_to_enum event.kind);
    true
  )
;;
