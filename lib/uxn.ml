type machine =
  { mutable pc : int
  ; memory : Memory.t
  ; devices : Devices.t
  ; work_stack : Stack.t
  ; ret_stack : Stack.t
  }

let make_machine (program : int array) =
  let ram = Memory.make ~n:0x10_000 in
  let m =
    { pc = 0x100
    ; memory = ram
    ; devices = Devices.make ~ram
    ; work_stack = Stack.make ~size:0x100
    ; ret_stack = Stack.make ~size:0x100
    }
  in
  Memory.load ram program ~at:m.pc;
  m
;;

type stack_wrapper =
  { stack : Stack.t
  ; pop : unit -> int
  ; push : int -> unit
  ; restore : unit -> unit
  }

let make_stack_wrapper (opcode : Opcodes.t) stack =
  let saved = Stack.get_ptr stack in
  { stack
  ; pop = (fun () -> if opcode.short then Stack.pop_short stack else Stack.pop_byte stack)
  ; push =
      (fun v -> if opcode.short then Stack.push_short stack v else Stack.push_byte stack v)
  ; restore = (fun () -> if opcode.keep then Stack.set_ptr stack saved)
  }
;;

type step_result =
  | Continue
  | Break

let step m =
  let byte = Memory.read_byte m.memory m.pc in
  let opcode = Opcodes.decode byte in
  m.pc <- m.pc + 1;

  let work = make_stack_wrapper opcode m.work_stack in
  let ret = make_stack_wrapper opcode m.ret_stack in
  let src, dst = if opcode.return then (ret, work) else (work, ret) in

  let jump_to addr =
    let addr = if not opcode.short then Utils.to_signed_byte addr else addr in
    m.pc <- (addr + if opcode.short then 0 else m.pc)
  in

  let pop_discard (ctx : stack_wrapper) n =
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
        let is_zero_page = opcode.base = Opcodes.LDZ || opcode.base = Opcodes.STZ in
        let addr =
          match opcode.base with
          | Opcodes.LDZ | Opcodes.STZ -> Stack.pop_byte src.stack
          | Opcodes.LDR | Opcodes.STR ->
            (m.pc + Utils.to_signed_byte (Stack.pop_byte src.stack)) land 0xffff
          | Opcodes.LDA | Opcodes.STA -> Stack.pop_short src.stack
          | _ -> assert false
        in
        let mask = if is_zero_page then Some 0xff else None in
        let value =
          if is_load
          then Memory.read ?mask m.memory addr ~short:opcode.short
          else src.pop ()
        in
        src.restore ();
        if is_load
        then src.push value
        else Memory.write ?mask m.memory addr ~value ~short:opcode.short
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
        let value = Memory.read m.memory m.pc ~short:opcode.short in
        src.push value;
        if opcode.short then m.pc <- m.pc + 2 else m.pc <- m.pc + 1
      | Opcodes.DEI ->
        let port = Stack.pop_byte src.stack in
        src.restore ();
        let value = Devices.read m.devices ~port ~short:opcode.short in
        src.push value
      | Opcodes.DEO ->
        let port = Stack.pop_byte src.stack in
        let value = src.pop () in
        src.restore ();
        Devices.write m.devices ~port ~value ~short:opcode.short
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

let rec process_args m args =
  let feed_arg arg ~final =
    String.iter
      (fun c ->
         Devices.push_input m.devices ~data:(Char.code c) ~kind:Devices.Console.Argument
       )
      arg;
    Devices.push_input
      m.devices
      ~data:(Char.code '\n')
      ~kind:(if final then Devices.Console.End else Devices.Console.ArgumentSpacer);
    while Devices.prepare_input m.devices do
      m.pc <- Devices.console_vector m.devices;
      run_until_break m
    done
  in
  match args with
  | hd :: tl ->
    feed_arg hd ~final:(tl = []);
    process_args m tl
  | [] -> ()
;;

let run m args =
  m.pc <- 0x100;
  Devices.write
    m.devices
    ~port:0x17
    ~value:(if List.length args > 1 then 1 else 0)
    ~short:false;
  run_until_break m;
  process_args m args
;;
