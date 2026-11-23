module type Console = sig
  include Uxn.Device

  type input_type =
    | Stdin
    | Argument
    | ArgumentSpacer
    | End

  val push : t -> data:int -> kind:input_type -> unit
  val prepare : t -> ports:Memory.t -> bool
end
