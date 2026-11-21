type t = { ram : Memory.t }

let create ~ram = { ram }
let read _ ~port:_ ~current = current

let write t ~port ~value =
  match port with
  (* System/metadata *)
  | 0x06 ->
    let buf = Buffer.create 64 in
    let rec loop i =
      let char = Memory.read_byte t.ram i in
      if char <> 0
      then (
        Buffer.add_int8 buf char;
        loop (i + 1)
      )
    in
    loop (value + 1);
    print_endline (Buffer.contents buf)
  (* System/state *)
  | 0x0f -> if value <> 0 then exit (value land 0x7f)
  | _ -> ()
;;
