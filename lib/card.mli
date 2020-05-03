type content =
  | File of string option * string
  | Plain of string

type t = {
  id : string;
  content : content; [@printer pp_content]
  box : int;
  deck : string;
  last_reviewed_at : float;
}
[@@deriving show, yojson]

module Id : sig
  type t = string

  val generate : (t -> bool) -> t
end

val generate_id : string -> string

(** [create id deck content box_id at] creates a new card in the [deck] with an
    [id] a [name] and a [content] [at] the given time *)
val create : string -> ?deck:string -> content -> float -> (t, string) result

val title : t -> string

val pp_content : Format.formatter -> content -> unit

val content_to_yojson : content -> Yojson.t

val content_of_yojson : Yojson.Safe.t -> (content, string) result

(** Card rating during a practice session *)
module Rating : sig
  type t =
    | Bad  (** The card is moved to the first box *)
    | Again  (** The card is not moved *)
    | Good  (** The card is graduated to the next box *)
    | Easy  (** The card is moved into the last box *)
  [@@deriving show]

  val of_string : string -> (t, string) result

  val to_string : t -> string
end
