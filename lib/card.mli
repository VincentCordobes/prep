type t = {
  id: string; 
  content: string; 
  last_reviewed_at: float
} [@@deriving show, yojson]

module Id : sig
  type t = string
  val generate : (t -> bool) -> t
end

val generate_id: string -> string

val create: string ->  string -> (t, string) result
(** [create id content] creates a new card with an [id] a [name] and a [content] *)

val title: t -> string


module Rating : sig
  type t = Bad | Again | Good | Easy
  [@@deriving show]
  (** Rating of the card during a practice session:
      `Bad:   We made some mistakes we have to repeat it again.
              The card is moved to the first box 
      `Again: Little mistakes.
              The card is moved down
      `Good:  We had to think but we got it right. 
              The card is graduated.
      `Easy:  No hesitation, no mistake. 
              The card is moved into the last box *)

  val from_int : int -> (t, string) result

  val of_string : string -> (t, string) result

  val to_string : t -> string 
end
