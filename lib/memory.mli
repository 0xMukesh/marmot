type t

val make : n:int -> t
val data : t -> int array
val read_byte : ?mask:int -> t -> int -> int
val set_byte : ?mask:int -> t -> int -> int -> unit
val read_short : ?mask:int -> t -> int -> int
val set_short : ?mask:int -> t -> int -> int -> unit
