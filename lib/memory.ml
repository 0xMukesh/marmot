type t = int array

let make ~n = Array.init n (fun _ -> 0)
let read_byte m i = m.(i)
let read_short m i = Utils.read_short m.(i + 1) m.(i)
let set_byte m i v = m.(i) <- v

let set_short m i v =
  let hi, lo = Utils.split_short v in
  m.(i) <- lo;
  m.(i + 1) <- hi
;;
