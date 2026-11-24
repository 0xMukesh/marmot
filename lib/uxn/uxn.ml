module type Device = sig
  type t

  val make : ram:Memory.t -> t
  val read : t -> port:int -> current:int -> int
  val write : t -> port:int -> value:int -> unit
end

module type S = sig
  type machine
  type device

  type step_result =
    | Continue
    | Break

  val make : unit -> machine
  val set_pc : machine -> int -> unit
  val device : machine -> device
  val ports : machine -> Memory.t
  val read_port : machine -> port:int -> short:bool -> int
  val write_port : machine -> port:int -> value:int -> short:bool -> unit
  val load_program : machine -> int array -> unit
  val check_halt : machine -> unit
  val step : machine -> step_result
end

module Make (D : Device) : S with type device = D.t = struct
  type machine =
    { mutable pc : int
    ; memory : Memory.t
    ; ports : Memory.t
    ; work_stack : Stack.t
    ; ret_stack : Stack.t
    ; device : D.t
    }

  type device = D.t

  type step_result =
    | Continue
    | Break

  let make () =
    let ram = Memory.make ~n:0x10_000 in
    { pc = 0x0
    ; memory = ram
    ; ports = Memory.make ~n:0x100
    ; work_stack = Stack.make ~size:0x100
    ; ret_stack = Stack.make ~size:0x100
    ; device = D.make ~ram
    }
  ;;

  let set_pc m pc = m.pc <- pc
  let device m = m.device
  let ports m = m.ports
  let read_port m ~port ~short = Memory.read m.ports ~at:port ~short
  let write_port m ~port ~value ~short = Memory.write m.ports ~at:port ~value ~short

  let load_program m program =
    set_pc m 0x100;
    Memory.load m.memory program ~at:0x100
  ;;

  let check_halt m =
    let value = Memory.read_byte m.ports ~at:0x0f in
    if value <> 0 then exit (value land 0x7f)
  ;;

  let step m =
    let byte = Memory.read_byte m.memory ~at:m.pc in
    let opcode = Opcodes.decode byte in
    m.pc <- m.pc + 1;

    let src, dst =
      if opcode.return then (m.ret_stack, m.work_stack) else (m.work_stack, m.ret_stack)
    in
    let saved = Stack.get_ptr src in

    let push stack value =
      if opcode.short then Stack.push_short stack value else Stack.push_byte stack value
    in
    let pop stack =
      if opcode.short then Stack.pop_short stack else Stack.pop_byte stack
    in
    let restore stack = if opcode.keep then Stack.set_ptr stack saved in
    let jump_to addr =
      let addr = if not opcode.short then Utils.to_signed_byte addr else addr in
      m.pc <- (addr + if opcode.short then 0 else m.pc)
    in

    let exec_op (opcode : Opcodes.t) =
      match opcode.base with
      | Opcodes.INC ->
        let v = pop src in
        restore src;
        push src (v + 1)
      | Opcodes.POP ->
        ignore (pop src);
        restore src
      | Opcodes.NIP ->
        let b = pop src in
        ignore (pop src);
        restore src;
        push src b
      | Opcodes.SWP ->
        let b = pop src in
        let a = pop src in
        restore src;
        push src b;
        push src a
      | Opcodes.ROT ->
        let c = pop src in
        let b = pop src in
        let a = pop src in
        restore src;
        push src b;
        push src c;
        push src a
      | Opcodes.DUP ->
        let a = pop src in
        restore src;
        push src a;
        push src a
      | Opcodes.OVR ->
        let b = pop src in
        let a = pop src in
        restore src;
        push src a;
        push src b;
        push src a
      | Opcodes.EQU | Opcodes.NEQ | Opcodes.GTH | Opcodes.LTH ->
        let compare =
          match opcode.base with
          | Opcodes.EQU -> ( = )
          | Opcodes.NEQ -> ( <> )
          | Opcodes.GTH -> ( > )
          | Opcodes.LTH -> ( < )
          | _ -> assert false
        in
        let b = pop src in
        let a = pop src in
        restore src;
        Stack.push_byte src (if compare a b then 1 else 0)
      | Opcodes.JMP ->
        let addr = pop src in
        restore src;
        jump_to addr
      | Opcodes.JCN ->
        let addr = pop src in
        let cond = Stack.pop_byte src in
        restore src;
        if cond <> 0 then jump_to addr
      | Opcodes.JSR ->
        Stack.push_short dst m.pc;
        let addr = pop src in
        restore src;
        jump_to addr
      | Opcodes.STH ->
        let value = pop src in
        restore src;
        push dst value
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
          | Opcodes.LDZ | Opcodes.STZ -> Stack.pop_byte src
          | Opcodes.LDR | Opcodes.STR ->
            (m.pc + Utils.to_signed_byte (Stack.pop_byte src)) land 0xffff
          | Opcodes.LDA | Opcodes.STA -> Stack.pop_short src
          | _ -> assert false
        in
        let mask = if is_zero_page then Some 0xff else None in
        let value =
          if is_load
          then Memory.read ?mask m.memory ~at:addr ~short:opcode.short
          else pop src
        in
        restore src;
        if is_load
        then push src value
        else Memory.write ?mask m.memory ~at:addr ~value ~short:opcode.short
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
        let b = pop src in
        let a = pop src in
        restore src;
        let result = if opcode.base = Opcodes.DIV && b = 0 then 0 else f a b in
        push src result
      | Opcodes.SFT ->
        let shift = Stack.pop_byte src in
        let a = pop src in
        restore src;
        let bits_to_right = shift land 0x0f in
        let bits_to_left = (shift land 0xf0) lsr 4 in
        let result = (a lsr bits_to_right) lsl bits_to_left in
        push src result
      | Opcodes.JCI | Opcodes.JMI | Opcodes.JSI ->
        let offset = Memory.read_short m.memory ~at:m.pc in
        let jump_imm () = m.pc <- (m.pc + 2 + offset) land 0xffff in
        ( match opcode.base with
          | Opcodes.JCI ->
            let cond = Stack.pop_byte src in
            restore src;
            if cond <> 0 then jump_imm () else m.pc <- m.pc + 2
          | Opcodes.JMI -> jump_imm ()
          | Opcodes.JSI ->
            Stack.push_short m.ret_stack (m.pc + 2);
            jump_imm ()
          | _ -> assert false
        )
      | Opcodes.LIT ->
        let value = Memory.read m.memory ~at:m.pc ~short:opcode.short in
        push src value;
        if opcode.short then m.pc <- m.pc + 2 else m.pc <- m.pc + 1
      | Opcodes.DEI ->
        let port = Stack.pop_byte src in
        restore src;
        let read_port p =
          let current = Memory.read m.ports ~at:p ~short:false in
          D.read m.device ~port:p ~current
        in
        let value =
          if opcode.short
          then (
            let hi = read_port port in
            let lo = read_port (port + 1) in
            Utils.read_short hi lo
          )
          else read_port port
        in
        push src value
      | Opcodes.DEO ->
        let port = Stack.pop_byte src in
        let value = pop src in
        restore src;
        let write_port p v =
          Memory.write m.ports ~at:p ~value:v ~short:false;
          D.write m.device ~port:p ~value:v
        in
        if opcode.short
        then (
          let hi, lo = Utils.split_short value in
          write_port port hi;
          write_port (port + 1) lo
        )
        else write_port port value
      | _ -> assert false
    in

    match opcode.base with
    | Opcodes.BRK -> Break
    | _ ->
      exec_op opcode;
      Continue
  ;;
end
