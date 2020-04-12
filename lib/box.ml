type t = {
  interval: Interval.t;
} [@@deriving show, yojson]

let create interval = {interval}

let equals a b = Interval.equals a.interval b.interval
