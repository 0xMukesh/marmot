let handle_out port value memory device =
  match port with
  | 0x00 -> () (* System/unused *)
  | 0x06 ->
    (* System/metadata *)
    let start = value + 1 in
    let buf = Buffer.create 64 in
    let rec loop i =
      let char = Memory.read_byte memory i in
      if char = 0
      then ()
      else (
        Buffer.add_int8 buf char;
        loop (i + 1)
      )
    in
    loop start;
    buf |> Buffer.to_bytes |> Bytes.to_string |> print_endline
  | 0x0f ->
    (* System/state *)
    if value <> 0
    then (
      let code = value land 0x7f in
      exit code
    )
  | 0x10 | 0x11 -> Memory.set_byte device port value
  | 0x18 ->
    (* Console/write *)
    print_char (Char.chr value);
    flush stdout
  | 0x19 ->
    (* Console/error *)
    prerr_char (Char.chr value);
    flush stderr
  | 0xff -> () (* unused *)
  | _ -> failwith (Printf.sprintf "device out is not yet implemented for 0x%02x" port)
;;
