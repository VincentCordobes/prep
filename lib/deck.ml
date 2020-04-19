type t = {
  id : string;
  boxes : Box.t list;
  deck_id : (string option[@default None]);
}
[@@deriving show, yojson]
(** Decks group cards together. It is useful when we want to review them 
   separately. A practice session may start with a specific deck *)

let default_id = "default"

let default_boxes =
  [
    Box.create @@ Day 3;
    Box.create @@ Week 1;
    Box.create @@ Day 8;
    Box.create @@ Week 6;
  ]

let create ?(id = default_id) ?(deck_id = None) ?(boxes = default_boxes) () =
  { id; deck_id; boxes }

let add_box box deck =
  let boxes =
    List.sort
      (fun a b -> Interval.compare Box.(a.interval) Box.(b.interval))
      (box :: deck.boxes)
  in
  { deck with boxes }
