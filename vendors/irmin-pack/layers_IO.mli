module type S = sig
  type t

  val v : string -> t

  val close : t -> unit

  val read_flip : t -> bool

  val write_flip : bool -> t -> unit

  val read_generation : t -> int64

  val write_generation : int64 -> t -> unit
end

module Unix : S
