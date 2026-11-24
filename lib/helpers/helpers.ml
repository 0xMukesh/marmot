let read_rom_file ~path =
  let program = Array.make 0xff00 0 in
  let ic = In_channel.open_bin path in
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
