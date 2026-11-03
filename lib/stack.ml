type t =
  { bytes : int array
  ; mutable ptr : int (* points to next slot *)
  }

let make ~size = { bytes = Array.init size (fun _ -> 0); ptr = 0 }

let push_byte stack v =
  if stack.ptr >= Array.length stack.bytes then failwith "stack overflow";
  stack.ptr <- stack.ptr + 1;
  stack.bytes.(stack.ptr) <- v
;;

let pop_byte stack =
  if stack.ptr = 0 then failwith "stack underflow";
  let v = stack.bytes.(stack.ptr) in
  stack.ptr <- stack.ptr - 1;
  v
;;

let push_short stack v =
  if stack.ptr + 2 >= Array.length stack.bytes then failwith "stack overflow";
  let hi, lo = Utils.split_short v in
  push_byte stack hi;
  push_byte stack lo
;;

let pop_short stack =
  if stack.ptr < 2 then failwith "stack underflow";
  let lo = pop_byte stack in
  let hi = pop_byte stack in
  Utils.read_short hi lo
;;
