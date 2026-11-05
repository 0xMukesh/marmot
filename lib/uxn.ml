type machine =
  { mutable pc : int
  ; memory : Memory.t
  ; device : Memory.t
  ; work_stack : Stack.t
  ; ret_stack : Stack.t
  }

type step_ctx =
  { stack : Stack.t
  ; pop : unit -> int
  ; push : int -> unit
  ; restore : unit -> unit
  }

type step_result =
  | Continue
  | Break

let make_machine program =
  let m =
    { pc = 0x100
    ; memory = Memory.make ~n:0x10_000
    ; device = Memory.make ~n:0x100
    ; work_stack = Stack.make ~size:0x100
    ; ret_stack = Stack.make ~size:0x100
    }
  in
  Array.blit program 0 (Memory.data m.memory) m.pc (Array.length program);
  m
;;

let make_ctx (opcode : Opcodes.t) stack =
  let saved = Stack.get_ptr stack in
  { stack
  ; pop = (fun () -> if opcode.short then Stack.pop_short stack else Stack.pop_byte stack)
  ; push =
      (fun v -> if opcode.short then Stack.push_short stack v else Stack.push_byte stack v)
  ; restore = (fun () -> if opcode.keep then Stack.set_ptr stack saved)
  }
;;

let console_vector m = Memory.read_short m.device 0x10

let handle_device_out m port value =
  let port = port land 0xff in
  let value = value land 0xff in
  match port with
  | 0x18 ->
    (* Console/write *)
    print_char (Char.chr value);
    flush stdout
  | 0x19 ->
    (* Console/error *)
    prerr_char (Char.chr value);
    flush stderr
  | _ -> Memory.set_byte m.device port value
;;

let step m =
  let byte = Memory.read_byte m.memory m.pc in
  let opcode = Opcodes.decode byte in
  m.pc <- m.pc + 1;

  let work = make_ctx opcode m.work_stack in
  let ret = make_ctx opcode m.ret_stack in
  let src, dst = if opcode.return then (ret, work) else (work, ret) in

  let mem_set_value ?mask mem addr value =
    if opcode.short
    then Memory.set_short ?mask mem addr value
    else Memory.set_byte ?mask mem addr value
  in
  let mem_read_value ?mask mem addr =
    if opcode.short
    then Memory.read_short ?mask mem addr
    else Memory.read_byte ?mask mem addr
  in
  let jump_to addr =
    let addr = if not opcode.short then Utils.to_signed_byte addr else addr in
    m.pc <- (addr + if opcode.short then 0 else m.pc)
  in
  let pop_discard ctx n =
    let rec loop i =
      if i <= 0
      then ()
      else (
        ignore (ctx.pop ());
        loop (i - 1)
      )
    in
    loop n
  in

  if opcode.base = Opcodes.BRK
  then Break
  else (
    ( match opcode.base with
      | Opcodes.INC ->
        let value = src.pop () in
        src.restore ();
        let result = value + 1 in
        src.push result
      | Opcodes.POP ->
        pop_discard src 1;
        src.restore ()
      | Opcodes.NIP ->
        let b = src.pop () in
        ignore (src.pop ());
        src.restore ();
        src.push b
      | Opcodes.SWP ->
        let b = src.pop () in
        let a = src.pop () in
        src.restore ();
        src.push b;
        src.push a
      | Opcodes.ROT ->
        let c = src.pop () in
        let b = src.pop () in
        let a = src.pop () in
        src.restore ();
        src.push b;
        src.push c;
        src.push a
      | Opcodes.DUP ->
        let a = src.pop () in
        src.restore ();
        src.push a;
        src.push a
      | Opcodes.OVR ->
        let b = src.pop () in
        let a = src.pop () in
        src.restore ();
        src.push a;
        src.push b;
        src.push a
      | Opcodes.EQU | Opcodes.NEQ | Opcodes.GTH | Opcodes.LTH ->
        let compare =
          match opcode.base with
          | Opcodes.EQU -> ( = )
          | Opcodes.NEQ -> ( <> )
          | Opcodes.GTH -> ( > )
          | Opcodes.LTH -> ( < )
          | _ -> assert false
        in
        let b = src.pop () in
        let a = src.pop () in
        src.restore ();
        Stack.push_byte src.stack (if compare a b then 1 else 0)
      | Opcodes.JMP ->
        let addr = src.pop () in
        src.restore ();
        jump_to addr
      | Opcodes.JCN ->
        let addr = src.pop () in
        let cond = Stack.pop_byte src.stack in
        src.restore ();
        if cond <> 0 then jump_to addr
      | Opcodes.JSR ->
        Stack.push_short dst.stack m.pc;
        let addr = src.pop () in
        src.restore ();
        jump_to addr
      | Opcodes.STH ->
        let value = src.pop () in
        src.restore ();
        dst.push value
      | Opcodes.LDZ | Opcodes.LDR | Opcodes.LDA | Opcodes.STZ | Opcodes.STR | Opcodes.STA
        ->
        let is_load =
          opcode.base = Opcodes.LDZ
          || opcode.base = Opcodes.LDR
          || opcode.base = Opcodes.LDA
        in
        let addr =
          match opcode.base with
          | Opcodes.LDZ | Opcodes.STZ -> Stack.pop_byte src.stack
          | Opcodes.LDR | Opcodes.STR ->
            (m.pc + Utils.to_signed_byte (Stack.pop_byte src.stack)) land 0xffff
          | Opcodes.LDA | Opcodes.STA -> Stack.pop_short src.stack
          | _ -> assert false
        in
        let mask =
          if opcode.base = Opcodes.LDZ || opcode.base = Opcodes.STZ
          then Some 0xff
          else None
        in
        let value =
          if not is_load then src.pop () else mem_read_value ?mask m.memory addr
        in
        src.restore ();
        if is_load then src.push value else mem_set_value ?mask m.memory addr value
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
        let b = src.pop () in
        let a = src.pop () in
        src.restore ();
        let result = if opcode.base = Opcodes.DIV && b = 0 then 0 else f a b in
        src.push result
      | Opcodes.SFT ->
        let shift = Stack.pop_byte src.stack in
        let a = src.pop () in
        src.restore ();
        let bits_to_right = shift land 0x0f in
        let bits_to_left = (shift land 0xf0) lsr 4 in
        let result = (a lsr bits_to_right) lsl bits_to_left in
        src.push result
      | Opcodes.JCI | Opcodes.JMI | Opcodes.JSI ->
        let offset = Memory.read_short m.memory m.pc in
        let jump_imm () = m.pc <- (m.pc + 2 + offset) land 0xffff in
        ( match opcode.base with
          | Opcodes.JCI ->
            let cond = Stack.pop_byte src.stack in
            src.restore ();
            if cond <> 0 then jump_imm () else m.pc <- m.pc + 2
          | Opcodes.JMI -> jump_imm ()
          | Opcodes.JSI ->
            Stack.push_short m.ret_stack (m.pc + 2);
            jump_imm ()
          | _ -> assert false
        )
      | Opcodes.LIT ->
        let value = mem_read_value m.memory m.pc in
        src.push value;
        if opcode.short then m.pc <- m.pc + 2 else m.pc <- m.pc + 1
      | Opcodes.DEI ->
        let port = Stack.pop_byte src.stack in
        src.restore ();
        let value = mem_read_value m.device port in
        src.push value
      | Opcodes.DEO ->
        let port = Stack.pop_byte src.stack in
        let value = src.pop () in
        src.restore ();
        mem_set_value m.device port value;
        handle_device_out m port value
      | _ -> assert false
    );
    Continue
  )
;;

let run_until_break m =
  let rec loop () =
    match step m with
    | Continue -> loop ()
    | Break -> ()
  in
  loop ()
;;

let run m args =
  m.pc <- 0x100;
  run_until_break m
;;
