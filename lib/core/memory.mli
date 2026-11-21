type t

val make : n:int -> t
val load : t -> int array -> at:int -> unit
val read_byte : ?mask:int -> t -> int -> int
val read_short : ?mask:int -> t -> int -> int
val read : ?mask:int -> t -> int -> short:bool -> int
val write_byte : ?mask:int -> t -> int -> value:int -> unit
val write_short : ?mask:int -> t -> int -> value:int -> unit
val write : ?mask:int -> t -> int -> value:int -> short:bool -> unit
