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

let read_byte ?mask mem loc = mem.data.(loc land unwrap_mask mask mem)

let read_short ?mask mem loc =
  Utils.read_short (read_byte ?mask mem loc) (read_byte ?mask mem (loc + 1))
;;

let read ?mask mem loc ~short =
  if short then read_short ?mask mem loc else read_byte ?mask mem loc
;;

let write_byte ?mask mem loc ~value =
  mem.data.(loc land unwrap_mask mask mem) <- value land 0xff
;;

let write_short ?mask mem loc ~value =
  let hi, lo = Utils.split_short value in
  write_byte ?mask mem loc ~value:hi;
  write_byte ?mask mem (loc + 1) ~value:lo
;;

let write ?mask mem loc ~value ~short =
  if short then write_short ?mask mem loc ~value else write_byte ?mask mem loc ~value
;;
