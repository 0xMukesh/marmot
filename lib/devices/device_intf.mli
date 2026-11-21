module type Device = sig
  type t

  val create : ram:Memory.t -> t
  val read : t -> port:int -> current:int -> int
  val write : t -> port:int -> value:int -> unit
end

module type Console = sig
  include Device

  type input_type =
    | Stdin
    | Argument
    | ArgumentSpacer
    | End

  val push : t -> data:int -> kind:input_type -> unit
  val prepare : t -> ports:Memory.t -> bool
end
