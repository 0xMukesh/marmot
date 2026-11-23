type t =
  { data : int array
  ; mask : int
  }

let make ~n =
  assert (n > 0 && n land (n - 1) = 0);
  { data = Array.make n 0; mask = n - 1 }
;;

let load t program ~at = Array.blit program 0 t.data at (Array.length program)

let unwrap_mask mask mem =
  match mask with
  | Some m -> m
  | None -> mem.mask
;;

let read_byte ?mask mem ~at = mem.data.(at land unwrap_mask mask mem)

let read_short ?mask mem ~at =
  Utils.read_short (read_byte ?mask mem ~at) (read_byte ?mask mem ~at:(at + 1))
;;

let read ?mask mem ~at ~short =
  if short then read_short ?mask mem ~at else read_byte ?mask mem ~at
;;

let write_byte ?mask mem ~at ~value =
  mem.data.(at land unwrap_mask mask mem) <- value land 0xff
;;

let write_short ?mask mem ~at ~value =
  let hi, lo = Utils.split_short value in
  write_byte ?mask mem ~at ~value:hi;
  write_byte ?mask mem ~at:(at + 1) ~value:lo
;;

let write ?mask mem ~at ~value ~short =
  if short then write_short ?mask mem ~at ~value else write_byte ?mask mem ~at ~value
;;
