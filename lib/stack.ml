type t =
  { bytes : int array
  ; mutable ptr : int (* points to next slot *)
  }

let make ~size = { bytes = Array.init size (fun _ -> 0); ptr = 0 }
let get_ptr stack = stack.ptr
let set_ptr stack ptr = stack.ptr <- ptr land 0xff

let push_byte stack v =
  stack.bytes.(stack.ptr) <- v land 0xff;
  stack.ptr <- (stack.ptr + 1) land 0xff
;;

let pop_byte stack =
  stack.ptr <- (stack.ptr - 1) land 0xff;
  stack.bytes.(stack.ptr) land 0xff
;;

let push_short stack v =
  let hi, lo = Utils.split_short v in
  push_byte stack hi;
  push_byte stack lo
;;

let pop_short stack =
  let lo = pop_byte stack in
  let hi = pop_byte stack in
  Utils.read_short hi lo
;;
