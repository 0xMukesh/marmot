type machine =
  { mutable pc : int
  ; memory : Memory.t
  ; device : Memory.t
  ; work_stack : Stack.t
  ; ret_stack : Stack.t
  }

type step_result =
  | Continue
  | Break

let init_machine program =
  let m =
    { pc = 0x100
    ; memory = Memory.make ~n:0x10_000
    ; device = Memory.make ~n:0x100
    ; work_stack = Stack.make ~size:0x100
    ; ret_stack = Stack.make ~size:0x100
    }
  in
  Array.blit program 0 m.memory m.pc (Array.length program);
  m
;;

let console_vector m = Memory.read_short m.device 0x10

let handle_device_out port value =
  let c = Char.chr (value land 0xff) in
  match port with
  | 0x18 ->
    (* Console/write *)
    print_char c;
    flush stdout
  | 0x19 ->
    (* Console/error *)
    prerr_char c;
    flush stderr
  | _ -> failwith (Printf.sprintf "cannot handle device out on 0x%02x port" port)
;;

let step m =
  let byte = Memory.read_byte m.memory m.pc in
  let opcode = Opcodes.decode byte in
  m.pc <- m.pc + 1;

  let stack = if opcode.return then m.ret_stack else m.work_stack in
  let stash_stack = if opcode.return then m.work_stack else m.ret_stack in
  (* peek / pop / push a short or a byte from the stack depending on whether short mode is enabled or not *)
  let peek_value () =
    if opcode.short then Stack.peek_short stack else Stack.peek_byte stack
  in
  let pop_value () =
    if opcode.short then Stack.pop_short stack else Stack.pop_byte stack
  in
  let push_value value =
    if opcode.short then Stack.push_short stack value else Stack.push_byte stack value
  in
  (* read / set a short or a byte from / to the memory depending on whether short mode is enabled or not *)
  let mem_read_value ?(mem = m.memory) i =
    if opcode.short then Memory.read_short mem i else Memory.read_byte mem i
  in
  let mem_set_value ?(mem = m.memory) i value =
    if opcode.short then Memory.set_short mem i value else Memory.set_byte mem i value
  in
  (* peek or pop value depending on whether keep mode is enabled or not *)
  let peek_or_pop value = if opcode.keep then peek_value () else pop_value () in
  (* pop N values out of the stack *)
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
  (* if short mode is enabled, jump using absolute address else relative offset *)
  let jump addr =
    if opcode.short
    then m.pc <- addr land 0xffff
    else m.pc <- (m.pc + Utils.to_signed_byte addr) land 0xffff
  in
  if opcode.base = Opcodes.BRK
  then Break
  else (
    ( match opcode.base with
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
        push_value b;
        push_value a
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
      | Opcodes.JMP ->
        let addr = peek_or_pop () in
        jump addr
      | Opcodes.JCN ->
        let addr = peek_or_pop () in
        let condition = peek_or_pop () in
        if condition <> 0 then jump addr
      | Opcodes.JSR ->
        let addr = peek_or_pop () in
        Stack.push_short stash_stack m.pc;
        jump addr
      | Opcodes.STH ->
        let value = peek_or_pop () in
        Stack.push_short stash_stack value
      | Opcodes.LDZ | Opcodes.LDR | Opcodes.LDA ->
        let addr =
          match opcode.base with
          | Opcodes.LDZ -> Stack.pop_byte stack
          | Opcodes.LDR -> m.pc + (Utils.to_signed_byte @@ Stack.pop_byte stack)
          | Opcodes.LDA -> Stack.pop_short stack
          | _ -> assert false
        in
        let value = mem_read_value addr in
        push_value value
      | Opcodes.STZ | Opcodes.STR | Opcodes.STA ->
        let addr =
          match opcode.base with
          | Opcodes.STZ -> Stack.pop_byte stack
          | Opcodes.STR -> m.pc + (Utils.to_signed_byte @@ Stack.pop_byte stack)
          | Opcodes.STA -> Stack.pop_short stack
          | _ -> assert false
        in
        let value = pop_value () in
        mem_set_value addr value
      | Opcodes.ADD
      | Opcodes.SUB
      | Opcodes.MUL
      | Opcodes.DIV
      | Opcodes.AND
      | Opcodes.ORA
      | Opcodes.EOR ->
        let f =
          match opcode.base with
          | Opcodes.ADD -> ( + )
          | Opcodes.SUB -> ( - )
          | Opcodes.MUL -> ( * )
          | Opcodes.DIV -> ( / )
          | Opcodes.AND -> ( land )
          | Opcodes.ORA -> ( lor )
          | Opcodes.EOR -> ( lxor )
          | _ -> assert false
        in
        let b = pop_value () in
        let a = pop_value () in
        push_value (f a b)
      | Opcodes.SFT ->
        let shift = Stack.pop_byte stack in
        let value = pop_value () in
        let bits_to_left = shift land 0xf in
        let bits_to_right = (shift land 0xf0) lsr 4 in
        let result = (value lsr bits_to_right) lsl bits_to_left in
        push_value result
      | Opcodes.DEI ->
        let port = Stack.pop_byte stack in
        let value = mem_read_value ~mem:m.device port in
        push_value value
      | Opcodes.DEO ->
        let port = Stack.pop_byte stack in
        let value = pop_value () in
        mem_set_value ~mem:m.device port value;
        handle_device_out port value
      | Opcodes.JCI ->
        let cond = Stack.pop_byte m.work_stack in
        let offset = mem_read_value m.pc in
        if cond <> 0 then m.pc <- m.pc + offset else m.pc <- m.pc + 2
      | Opcodes.JMI ->
        let offset = mem_read_value m.pc in
        m.pc <- m.pc + offset
      | Opcodes.JSI ->
        let offset = Memory.read_short m.memory m.pc in
        Stack.push_short m.ret_stack (m.pc + 2);
        m.pc <- m.pc + 2 + offset
      | Opcodes.LIT ->
        push_value (mem_read_value m.pc);
        if opcode.short then m.pc <- m.pc + 2 else m.pc <- m.pc + 1
      | _ -> assert false
    );
    Continue
  )
;;

let run m =
  m.pc <- 0x100;
  let rec loop () =
    match step m with
    | Continue -> loop ()
    | Break -> ()
  in
  loop ()
;;
