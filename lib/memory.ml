type t = int array

let make ~n = Array.init n (fun _ -> 0)
let at m i = m.(i)
let peek_byte m pc = m.(pc + 1)
let peek_short m pc = Utils.read_short m.(pc + 1) m.(pc + 2)
