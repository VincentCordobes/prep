type t = {id : string; content : string} [@@deriving show, yojson]

val create: string -> string -> (t, string) result

val title: t -> string

module Id : sig
  type t = string

  val generate : (t -> bool) -> t
end

module Review : sig
  type t

  val create : int -> (t, string) result

  val to_string : t -> int
end
