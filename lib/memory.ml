type t =
  { data : int array
  ; mask : int
  }

let make ~n = { data = Array.init n (fun _ -> 0); mask = n - 1 }
let read_byte m i = m.data.(i land m.mask)
let set_byte m i v = m.data.(i land m.mask) <- v land 0xff
let read_short m i = Utils.read_short (read_byte m i) (read_byte m (i + 1))

let set_short m i v =
  let hi, lo = Utils.split_short v in
  set_byte m i hi;
  set_byte m (i + 1) lo
;;
