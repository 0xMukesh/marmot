type machine =
  { mutable pc : int
  ; memory : Memory.t
  ; device : Memory.t
  ; work_stack : Stack.t
  ; ret_stack : Stack.t
  }

let init_machine program =
  { pc = 0x100
  ; memory = Memory.make ~n:0x10_000
  ; device = Memory.make ~n:0x100
  ; work_stack = Stack.make ~size:0x100
  ; ret_stack = Stack.make ~size:0x100
  }
;;

let eval m byte =
  let opcode = Opcodes.decode byte in
  let stack = if opcode.return then m.ret_stack else m.work_stack in
  let peek_value () =
    if opcode.short then Stack.peek_short stack else Stack.peek_byte stack
  in
  let pop_value () =
    if opcode.short then Stack.pop_short stack else Stack.pop_byte stack
  in
  let push_value value =
    if opcode.short then Stack.push_short stack value else Stack.push_byte stack value
  in
  let peek_or_pop value = if opcode.keep then peek_value () else pop_value () in
  let pop_discard ?(n = 1) () =
    let rec loop i =
      if i <= 0
      then ()
      else (
        ignore (pop_value ());
        loop (i - 1)
      )
    in
    loop n
  in
  match opcode.base with
  | Opcodes.BRK -> ()
  | Opcodes.INC ->
    let value = peek_or_pop () in
    let mask = if opcode.short then 0xffff else 0xff in
    push_value ((value + 1) land mask)
  | Opcodes.POP -> if opcode.keep then () else pop_discard ()
  | Opcodes.NIP ->
    let value = peek_value () in
    if not opcode.keep then pop_discard ~n:2 ();
    push_value value
  | Opcodes.SWP ->
    let b = peek_or_pop () in
    let a = peek_or_pop () in
    push_value a;
    push_value b
  | Opcodes.ROT ->
    let c = peek_or_pop () in
    let b = peek_or_pop () in
    let a = peek_or_pop () in
    push_value b;
    push_value c;
    push_value a
  | Opcodes.DUP ->
    let value = peek_or_pop () in
    push_value value;
    push_value value
  | Opcodes.OVR ->
    let b = peek_or_pop () in
    let a = peek_or_pop () in
    push_value a;
    push_value b;
    push_value a
  | Opcodes.EQU | Opcodes.NEQ | Opcodes.GTH | Opcodes.LTH ->
    let f =
      match opcode.base with
      | Opcodes.EQU -> ( = )
      | Opcodes.NEQ -> ( <> )
      | Opcodes.GTH -> ( > )
      | Opcodes.LTH -> ( < )
      | _ -> assert false
    in
    let b = peek_or_pop () in
    let a = peek_or_pop () in
    if f a b then push_value 1 else push_value 0
  | _ -> assert false
;;

let run machine =
  let byte = Memory.at machine.memory machine.pc in
  eval machine byte;
  machine.pc <- machine.pc + 1
;;
