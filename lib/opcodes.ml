type base_ops =
  | BRK [@value 0x00]
  | INC
  | POP
  | NIP
  | SWP
  | ROT
  | DUP
  | OVR
  | EQU
  | NEQ
  | GTH
  | LTH
  | JMP
  | JCN
  | JSR
  | STH
  | LDZ
  | STZ
  | LDR
  | STR
  | LDA
  | STA
  | DEI
  | DEO
  | ADD
  | SUB
  | MUL
  | DIV
  | AND
  | ORA
  | EOR
  | SFT
  | JCI
  | JMI [@value 0x40]
  | JSI [@value 0x60]
  | LIT [@value 0x80]
[@@deriving show { with_path = false }, enum, enumerate]

type t =
  { base : base_ops
  ; short : bool
  ; return : bool
  ; keep : bool
  }
[@@deriving make]

let show t =
  let suffixes =
    [ (t.short, "2"); (t.return, "r"); (t.keep, "k") ]
    |> List.filter_map (fun (enabled, suffix) -> if enabled then Some suffix else None)
  in
  show_base_ops t.base ^ String.concat "" suffixes
;;

let decode op =
  let decode_base op ~short ~return ~keep =
    match base_ops_of_enum op with
    | Some base -> make ~base ~short ~return ~keep
    | None -> failwith (Printf.sprintf "unknown opcode: %02x" op)
  in
  let offset, short, return, keep =
    match op with
    | n when n >= 0x21 && n <= 0x3f -> (0x20, true, false, false)
    | n when n >= 0x41 && n <= 0x5f -> (0x40, false, true, false)
    | n when n >= 0x61 && n <= 0x7f -> (0x60, true, true, false)
    | n when n >= 0x81 && n <= 0x9f -> (0x80, false, false, true)
    | n when n >= 0xa1 && n <= 0xbf -> (0xa0, true, false, true)
    | n when n >= 0xc1 && n <= 0xdf -> (0xc0, false, true, true)
    | n when n >= 0xe1 && n <= 0xff -> (0xe0, true, true, true)
    | 0xa0 -> (0x20, true, false, false) (* LIT2 *)
    | 0xc0 -> (0x40, false, true, false) (* LITr *)
    | 0xe0 -> (0x60, true, true, false) (* LIT2r *)
    | _ -> (0x00, false, false, false)
  in
  decode_base (op - offset) ~short ~return ~keep
;;

let%test _ =
  let base_cases =
    List.map (fun op -> (base_ops_to_enum op, show_base_ops op)) all_of_base_ops
  in
  let mode_cases =
    [ (0x21, "INC2")
    ; (0x42, "POPr")
    ; (0x63, "NIP2r")
    ; (0x84, "SWPk")
    ; (0xa5, "ROT2k")
    ; (0xc6, "DUPrk")
    ; (0xe7, "OVR2rk")
    ]
  in
  let lit_cases = [ (0xa0, "LIT2"); (0xc0, "LITr"); (0xe0, "LIT2r") ] in
  let checker l =
    let failed =
      List.filter_map
        (fun (op, literal) ->
           if show @@ decode op = literal then None else Some (Printf.sprintf "0x%02x" op)
         )
        l
    in
    if List.length failed = 0
    then true
    else failwith ("failed to decode opcodes: " ^ String.concat ", " failed)
  in
  let tests = [ base_cases; mode_cases; lit_cases ] in
  List.fold_left (fun acc v -> checker v && acc) true tests
;;
