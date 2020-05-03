(** A box is associated with a time interval--the duration between each practice
    session. *)
type t = { interval : Interval.t } [@@deriving show, yojson]

let create interval = { interval }

let equals a b = Interval.equals a.interval b.interval
