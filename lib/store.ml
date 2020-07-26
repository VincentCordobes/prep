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

let _patch_ids store =
  let cards =
    List.map store.cards ~f:(fun card ->
        Card.{ card with id = Card.Id.generate (Card.title card) })
  in
  { store with cards }

let save store =
  let boxes_json = to_yojson store in
  Yojson.Safe.to_file store_path boxes_json

let get_boxes ?deck store =
  let deck = match deck with Some deck -> deck | None -> store.current_deck in
  match List.find store.decks ~f:(fun d -> d.id = deck) with
  | Some deck -> deck.boxes
  | None -> []

let get_box (card : Card.t) store =
  List.nth_exn (get_boxes ~deck:card.deck store) card.box

let get_cards ?deck store =
  let deck = match deck with Some deck -> deck | None -> store.current_deck in
  List.filter store.cards ~f:(fun card -> card.deck = deck)

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

let find_card_by_id id store =
  let is_matching id card =
    let regexp = Re.Pcre.re ("^" ^ id) |> Re.compile in
    Re.execp regexp Card.(card.id)
  in
  List.filter store.cards ~f:(is_matching id)

let find_card_by_title text store =
  let is_matching text card =
    let regexp = Re.Pcre.re ~flags:[ `CASELESS ] text |> Re.compile in
    Re.execp regexp (Card.title card)
  in
  List.filter store.cards ~f:(is_matching text)

exception Card_not_found

exception Ambiguous_search of Card.t list

type card_search_error =
  | Card_not_found
  | Ambigous_card_id of Card.t list
  | Ambigous_card_name of Card.t list

let find_card text store =
  match find_card_by_id text store with
  | [] -> (
      match find_card_by_title text store with
      | [] -> Error Card_not_found
      | [ x ] -> Ok x
      | matches -> Error (Ambigous_card_name matches) )
  | [ x ] -> Ok x
  | matches -> Error (Ambigous_card_id matches)

(* [get_unambigous_short_id_length cards] finds the minimum length of ids so
   that cards are non-ambigous *)
let get_unambigous_short_id_length cards =
  let module Set = Set.Make (String) in
  let ids = cards |> List.map ~f:(fun card -> Card.(card.id)) in
  let rec loop len =
    if len >= 32 then
      32
    else
      let distinct_ids =
        ids |> List.map ~f:(fun id -> Str.first_chars id len) |> Set.of_list
      in
      if Set.cardinal distinct_ids = List.length cards then
        len
      else
        loop (len + 1)
  in
  loop 1

let find_card_exn card_id store =
  let print_ambigous_card_message cards =
    Fmt.(pf stderr "The most similar cards are\n");
    List.iter cards ~f:(fun card ->
        Fmt.(
          pf stderr "  * %a %s\n"
            Fmt.(styled `Yellow string)
            Card.(
              Id.to_short ~length:(get_unambigous_short_id_length cards) card.id)
            (Card.title card)));
    raise (Ambiguous_search cards)
  in
  match find_card card_id store with
  | Ok result -> result
  | Error Card_not_found ->
      Console.(print_error "No card found with id %a" yellow_s card_id);
      raise Card_not_found
  | Error (Ambigous_card_id cards) ->
      Console.(print_error "Several cards matches %a.\n" cyan_s card_id);
      print_ambigous_card_message cards
  | Error (Ambigous_card_name cards) ->
      Console.(print_error "Several cards matches id %a.\n" cyan_s card_id);
      print_ambigous_card_message cards

let exists card_id store =
  match find_card_by_id card_id store with [ _ ] -> true | _ -> false

let move_card_to date to_box card_id store =
  let card = find_card_exn card_id store in
  let boxes_count = get_boxes ~deck:card.deck store |> List.length in
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
