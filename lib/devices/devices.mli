module Console : sig
  type input_type =
    | Stdin
    | Argument
    | ArgumentSpacer
    | End
end

type t

val console_vector : t -> int
val push_input : t -> data:int -> kind:Console.input_type -> unit
val prepare_input : t -> bool
val make : ram:Memory.t -> t
val read : t -> port:int -> short:bool -> int
val write : t -> port:int -> value:int -> short:bool -> unit
