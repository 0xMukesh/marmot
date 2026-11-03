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

let eval m op =
  match op with
  | 0x00 -> () (* break *)
  | 0x20 ->
    (* jump conditional immediate *)
    let a = Stack.pop_byte m.work_stack in
    if a <> 0
    then (
      let p = Memory.peek_short m.memory m.pc in
      m.pc <- m.pc + p
    )
    else m.pc <- m.pc + 2
  | 0x40 ->
    (* jump immediate *)
    let p = Memory.peek_short m.memory m.pc in
    m.pc <- m.pc + p
  | 0x60 ->
    (* jump stash return immediate *)
    Stack.push_short m.ret_stack (m.pc + 2);
    let p = Memory.peek_short m.memory m.pc in
    m.pc <- m.pc + p
  | 0x80 ->
    (* literal *)
    ()
  | code -> failwith (Printf.sprintf "unknown opcode: %02x" code)
;;

let run machine =
  let opcode = Memory.at machine.memory machine.pc in
  eval machine opcode;
  machine.pc <- machine.pc + 1
;;
