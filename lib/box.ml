open Base

type t = {
  interval: Interval.t;
  cards: Card.t list;
} [@@deriving show, yojson]


let create ?(cards = []) interval  =
  {interval; cards}

let add card box = 
  {box with cards = card :: box.cards}

let set card_id value box =
  let cards =
    List.map box.cards ~f:(fun card ->
        if String.(card.id = card_id) then value else card)
  in
  {box with cards}

let remove card_id box =
  let cards = 
    List.filter box.cards ~f:(fun f -> 
        not String.(f.id = card_id)) 
  in
  {box with cards}

let equals a b =
  Interval.equals a.interval b.interval
