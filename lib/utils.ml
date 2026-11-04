let to_signed_byte byte = if byte land 0x80 <> 0 then byte - 0x100 else byte
let read_short hi lo = (hi lsr 8) lor lo

let split_short v =
  let hi = (v lsr 8) land 0xff in
  let lo = v land 0xff in
  (hi, lo)
;;
