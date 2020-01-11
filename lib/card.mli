type t = {id : string; content : string} [@@deriving show, yojson]

val create: string -> string -> (t, string) result

val title: t -> string

module Id : sig
  type t = string

  val generate : (t -> bool) -> t
end

module Rating : sig
  type t = Bad | Again | Good | Easy
  (** Rating of the card during a rehearsal session:
      `Bad:   We made some mistakes we have to repeat it again.
                  The card is moved to the first box 
      `Again: Little mistakes.
                  The card is moved down
      `Good:  We had to think but we got it right. 
                  The card is graduated.
      `Easy:  No hesitation, no mistake. 
                  The card is moved into the last box *)

  val from_int : int -> (t, string) result

  val to_string : t -> string 
end
