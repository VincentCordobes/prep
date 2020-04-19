module List = Base.List
module Hashtbl = Base.Hashtbl

type t = {
  cards : Card.t list; [@default []]
  decks : Deck.t list; [@default [ Deck.create () ]]
  current_deck : string; [@default Deck.default_id]
}
[@@deriving show, yojson { exn = true }]

let store_path =
  try Sys.getenv "STORE_PATH"
  with Not_found ->
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

let get_boxes store =
  match List.find store.decks ~f:(fun d -> d.id = store.current_deck) with
  | Some deck -> deck.boxes
  | None -> []

let add_box box store =
  let decks =
    match store.decks with
    | [] ->
        let deck = Deck.create ~boxes:[ box ] () in
        [ deck ]
    | _ ->
        List.map store.decks ~f:(fun deck ->
            if Deck.(deck.id) = store.current_deck then
              Deck.add_box box deck
            else
              deck)
  in
  { store with decks }

let add_deck (deck : Deck.t) store =
  {
    store with
    decks = List.filter store.decks ~f:(fun d -> d.id <> deck.id) @ [ deck ];
  }

let add card store =
  {
    store with
    cards = card :: List.filter store.cards ~f:(fun c -> c.id <> card.id);
  }

let set_card id card store =
  {
    store with
    cards = card :: List.filter store.cards ~f:(fun card -> card.id <> id);
  }

let all_cards store = store.cards

type find_card_error =
  | Card_not_found
  | Ambigous_card_id of Card.t list

let find_card ?(exact = false) card_id store =
  let partial_match a b =
    let regexp = Re.Pcre.re (String.lowercase_ascii a) |> Re.compile in
    Re.execp regexp (String.lowercase_ascii b)
  in
  let rec get_matches acc = function
    | [] -> acc
    | card :: tail ->
        if card_id = Card.(card.id) then
          [ card ]
        else if
          (not exact)
          && (partial_match card_id card.id || partial_match card.id card_id)
        then
          get_matches (card :: acc) tail
        else
          get_matches acc tail
  in
  let matches = get_matches [] store.cards in
  match matches with
  | [] -> Error Card_not_found
  | [ x ] -> Ok x
  | all_matches -> Error (Ambigous_card_id all_matches)

exception Card_not_found

exception Ambiguous_search of Card.t list

let find_card_exn ?(exact = false) card_id store =
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

let exists ?(exact = false) card_id store =
  let result = find_card ~exact card_id store in
  match result with Ok _ -> true | Error _ -> false

let move_card_to date to_box card_id store =
  let card = find_card_exn card_id store in
  let boxes_count = get_boxes store |> List.length in
  let box_exists = to_box >= 0 && to_box < boxes_count in
  let set_card_box to_box =
    set_card card.id { card with box = to_box; last_reviewed_at = date } store
  in
  if box_exists then
    set_card_box to_box
  else if to_box < 0 then
    set_card_box 0
  else if to_box >= boxes_count then
    set_card_box (boxes_count - 1)
  else
    store

let empty_store () = { cards = []; decks = []; current_deck = Deck.default_id }

let default_store () = empty_store () |> add_deck (Deck.create ())

let init ?(store = default_store ()) () =
  if Sys.file_exists store_path then
    ()
  else begin
    Util.mkdir_p (Filename.dirname store_path) 0o777;
    store |> save
  end
