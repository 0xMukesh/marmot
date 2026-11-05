type t =
  { data : int array
  ; mask : int
  }

let make ~n = { data = Array.init n (fun _ -> 0); mask = n - 1 }
let data m = m.data

let unwrap_mask mask m =
  match mask with
  | Some m -> m
  | None -> m.mask
;;

let read_byte ?mask m i = m.data.(i land unwrap_mask mask m)
let set_byte ?mask m i v = m.data.(i land unwrap_mask mask m) <- v land 0xff

let read_short ?mask m i =
  Utils.read_short (read_byte ?mask m i) (read_byte ?mask m (i + 1))
;;

let set_short ?mask m i v =
  let hi, lo = Utils.split_short v in
  set_byte ?mask m i hi;
  set_byte ?mask m (i + 1) lo
;;
