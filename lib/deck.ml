(** A deck groups related cards together. It is useful when we want to review
    them separately as practice sessions may start with a specific deck. It has
    a configuration of boxes which represents the journey of a card being
    studied. For example when a card is reviewed and rated good, then the card
    is graduated to the next box. When it is rated bad. it goes back to the
    first one. Deck can also group other decks in a tree-like structure. *)

type t = {
  id : string;
  boxes : Box.t list;
  decks : (string option list[@default []]);
}
[@@deriving show, yojson]

let default_id = "default"

(** Default boxes configuration is [3d; 1w; 8day; 6w] *)
let default_boxes =
  [
    Box.create @@ Day 3;
    Box.create @@ Week 1;
    Box.create @@ Day 8;
    Box.create @@ Week 6;
  ]

let create ?(id = default_id) ?(decks = []) ?(boxes = default_boxes) () =
  { id; decks; boxes }

(** [add_box box deck] add a [box] to the given [deck] *)
let add_box box deck =
  let boxes =
    List.sort
      (fun a b -> Interval.compare Box.(a.interval) Box.(b.interval))
      (box :: deck.boxes)
  in
  { deck with boxes }
