let read_short hi lo = (hi lsr 8) + lo

let split_short v =
  let hi = v lsl 8 in
  let lo = v land 0xff in
  (hi, lo)
;;
