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

val create : string -> ?deck:string -> content -> float -> (t, string) result
(** [create id deck content box_id at] creates a new card in the [deck] 
    with an [id] a [name] and a [content] [at] the given time *)

val title : t -> string

val pp_content : Format.formatter -> content -> unit

val content_to_yojson : content -> Yojson.t

val content_of_yojson : Yojson.Safe.t -> (content, string) result

module Rating : sig
  (** Rating of the card during a practice session:
      `Bad:   We made some mistakes we have to repeat it again.
              The card is moved to the first box 
      `Again: Little mistakes.
              The card is moved down
      `Good:  We had to think but we got it right. 
              The card is graduated.
      `Easy:  No hesitation, no mistake. 
              The card is moved into the last box *)
  type t =
    | Bad
    | Again
    | Good
    | Easy
  [@@deriving show]

  val from_int : int -> (t, string) result

  val of_string : string -> (t, string) result

  val to_string : t -> string
end
