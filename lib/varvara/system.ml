type t = { ram : Memory.t }

let make ~ram = { ram }

let read t ~port ~current =
  match port with
  | _ -> current
;;

let write t ~port ~value =
  match port with
  (* System/metadata *)
  | 0x06 ->
    let buf = Buffer.create 64 in
    let rec loop i =
      let char = Memory.read_byte t.ram ~at:i in
      if char <> 0
      then (
        Buffer.add_int8 buf char;
        loop (i + 1)
      )
    in
    loop (value + 1);
    print_endline (Buffer.contents buf)
  | _ -> ()
;;
