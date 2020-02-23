module List = Base.List
module Hashtbl = Base.Hashtbl

type t = {boxes: Box.t list} 
[@@deriving show, yojson {exn = true}]


let store_path = 
  try Sys.getenv "STORE_PATH" with
  | Not_found -> 
    let app_dir = ".config/prep" in
    let store_name = "store.json" in
    let home = Sys.getenv "HOME" in
    Fmt.str "%s/%s/%s" home app_dir store_name

let load () = 
  let json_value = Yojson.Safe.from_file store_path in
  let boxes = of_yojson_exn json_value in
  boxes

let save store =
  let boxes_json = to_yojson store in
  Yojson.Safe.to_file store_path boxes_json


let add_box box store =
  {
    boxes =
      List.sort (box :: store.boxes) ~compare:(fun a b ->
          Interval.compare a.interval b.interval);
  }

let add card ?(at = 0) store =
  let rec add i boxes =
    match boxes with
    | [] -> [Box.add card (Box.create (Interval.Day 1))]
    | boxe :: boxes ->
      if i = at then Box.add card boxe :: boxes
      else boxe :: add (i - 1) boxes
  in
  let boxes = add 0 store.boxes in
  {boxes}

let all_cards store =
  List.concat_mapi store.boxes ~f:(fun i box ->
      box.cards |> List.map ~f:(fun card -> (i, card)))

type find_card_error =
  | Card_not_found
  | Ambigous_card_id of Card.t list

let find_card ?(exact = false) card_id store =
  let partial_match a b =
    Str.string_partial_match
      (Str.regexp (String.lowercase_ascii a))
      (String.lowercase_ascii b) 0
  in
  let rec get_matches acc = function
    | [] -> acc
    | (box_id, card) :: tail ->
        if card_id = Card.(card.id) then [(box_id, card)]
        else if (not exact) && partial_match card_id card.id then
          get_matches ((box_id, card) :: acc) tail
        else get_matches acc tail
  in
  let matches = get_matches [] (all_cards store) in
  match matches with
  | [] -> Error Card_not_found
  | [x] -> Ok x
  | all_matches ->
      let cards = List.map all_matches ~f:(fun (_, card) -> card) in
      Error (Ambigous_card_id cards)


exception Card_not_found
exception Ambiguous_search of Card.t list

let find_card_exn ?(exact=false) card_id store =
  match find_card ~exact card_id store with
  | Ok result -> result
  | Error Card_not_found ->
    Console.(print_error "No card found with id %a" yellow_s card_id);
    raise Card_not_found
  | Error (Ambigous_card_id cards) ->
    Console.(print_error "Several cards matches id %a.\n" cyan_s card_id);
    Fmt.(pf stderr "The most similar cards are\n");
    List.iter cards ~f:(fun card ->
        Fmt.(pf stderr "  * %a\n" Console.yellow_s card.id));
    raise (Ambiguous_search cards)


let exists ?(exact=false) card_id store =
  let result = find_card ~exact card_id store in
  match result with
  | Ok _ -> true
  | Error _ -> false


let move_card_to to_box card_id store =
  let boxes_count = List.length store.boxes in
  let from_box, card = find_card_exn card_id store in
  let to_box = 
    if to_box < 0 then 0 
    else if to_box >= boxes_count then 
      (boxes_count - 1) 
    else to_box 
  in
  let boxes =
    List.mapi store.boxes ~f:(fun i box ->
        let box = 
          if i = from_box then Box.remove card.id box
          else box in

        if i = to_box then
          Box.add {card with last_reviewed_at = Unix.time ()} box
        else box
      )
  in
  {boxes}


let empty_store () =
  {boxes = []}

let default_store () =
  empty_store ()
  |> add_box @@ Box.create @@ Day 3
  |> add_box @@ Box.create @@ Week 1
  |> add_box @@ Box.create @@ Day 8
  |> add_box @@ Box.create @@ Week 6


let init () =
  if Sys.file_exists store_path then ()
  else
    begin
      Util.mkdir_p (Filename.dirname store_path) 0o777;
      default_store () |> save
    end
