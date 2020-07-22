open Base
open Cmdliner

let rec add ?(last_reviewed_at = Unix.time ()) ?(retry = false) content =
  let content =
    match content with
    | Some s -> if retry then Editor.edit (s ^ Editor.default_template) else s
    | None -> Editor.edit Editor.default_template
  in
  let store = Store.load () in

  let card_content = Card.Plain content in

  let id = Card.Id.generate (Card.title_of_content card_content) in

  let exists = Store.exists id store in
  if exists then (
    Fmt.pr "This name already exists. Press any key to continue...@.";
    Caml.(input_char Caml.stdin) |> ignore;
    add (Some content) ~retry:true )
  else
    match Card.create id card_content last_reviewed_at with
    | Ok card ->
        let updated_store = Store.add card store in
        Store.save updated_store;
        Fmt.pr "Card added (id: %s)\n" (Card.Id.to_short card.id)
    | Error msg -> failwith msg

let add_file ?(name = None) ?(last_reviewed_at = Unix.time ()) file =
  let open Caml in
  let store = Store.load () in

  let path =
    if Filename.is_relative file then
      Filename.concat (Sys.getcwd ()) file
    else
      file
  in
  let card_content = Card.File (name, path) in
  let id = Card.Id.generate (Card.title_of_content card_content) in
  let exists = Store.exists id store in
  if exists then
    Fmt.pr "This card already exists@."
  else
    match
      Card.create id ~deck:store.current_deck card_content last_reviewed_at
    with
    | Ok card ->
        store |> Store.add card |> Store.save;
        Fmt.pr "Card added (id: %s)\n" card.id
    | Error msg -> failwith msg

let add_box interval =
  let store = Store.load () in
  let box_exists =
    List.exists
      ~f:(fun box -> Interval.compare box.interval interval = 0)
      (Store.get_boxes store)
  in
  if box_exists then
    Console.(print_error "A box with interval %a already exists" green_s)
    @@ Interval.to_string interval
  else begin
    Store.save (Store.add_box (Box.create interval) store);
    Fmt.pr "Box added (repetitions every %a)" Console.green_s
      (Interval.to_string interval)
  end

let date_of_datetime dt =
  let open ISO8601.Permissive in
  date (string_of_date dt)

let next_review (interval : Interval.t) (card : Card.t) =
  Float.(
    let interval =
      let day_to_second n = of_int n * 24.0 * 60.0 * 60.0 in
      match interval with
      | Day n -> day_to_second n
      | Week n -> day_to_second n * 7.0
    in
    date_of_datetime card.last_reviewed_at + interval)

let print_cards_to_review now store cards =
  let open Card in
  let grouped_cards =
    cards
    |> List.map ~f:(fun card ->
           let box = Store.get_box card store in
           let date = next_review box.interval card in
           (date, card))
    |> List.sort ~compare:(fun (x, _) (y, _) -> Float.(x - y |> to_int))
    |> List.group ~break:(fun (x, _) (y, _) -> Float.(x <> y))
    |> List.map ~f:(fun group ->
           let cards = List.map group ~f:(fun (_, card) -> card) in
           let date, _ = List.hd_exn group in
           (date, cards))
  in

  let is_today date =
    let date = Unix.localtime date in
    let now = Unix.localtime now in
    date.tm_year = now.tm_year && date.tm_yday = now.tm_yday
  in

  let cards_to_review, futur_cards_to_review =
    let ( <= ) x y =
      let y = Unix.localtime y in
      let x = Unix.localtime x in
      if x.tm_year = y.tm_year then
        x.tm_yday <= y.tm_yday
      else
        x.tm_year < y.tm_year
    in
    List.partition_tf grouped_cards ~f:(fun (date, _cards) -> date <= now)
  in

  let pp_box ppf card = Fmt.pf ppf "#%d" (card.box + 1) in

  let pp_cards ppf cards =
    match cards with
    | [] -> Fmt.pf ppf "%a@." Fmt.(styled `Yellow string) "  --"
    | _ ->
        cards
        |> List.iteri ~f:(fun i card ->
               let indent = if i = 0 then "  " else "            " in
               Fmt.pf ppf "%s%a %s@." indent
                 (Fmt.styled `Green pp_box)
                 card (title card))
  in

  let pp_group pp_cards ppf (date, cards) =
    let color = if is_today date then `Yellow else `Faint in
    Fmt.pf ppf "%a%a"
      (Fmt.styled color ISO8601.Permissive.pp_date)
      date pp_cards cards
  in

  let cards_to_review =
    let today_empty =
      not (List.exists cards_to_review ~f:(fun (date, _) -> is_today date))
    in
    if today_empty then
      cards_to_review @ [ (now, []) ]
    else
      cards_to_review
  in

  let cards_to_print =
    match futur_cards_to_review with
    | [] -> cards_to_review
    | x :: _ -> cards_to_review @ [ x ]
  in

  if List.length cards > 0 then
    cards_to_print
    |> List.iteri ~f:(fun _ card_group ->
           (* if i <> 0 then Fmt.pr "\n"; *)
           Fmt.pr "%a" (pp_group pp_cards) card_group)
  else
    Fmt.pr "No card.\n"

let pp_cards ?interval ppf cards =
  let open Card in
  let open ISO8601.Permissive in
  let open Fmt in
  let pp_content ppf card =
    match interval with
    | None -> pf ppf "Box #%d" (card.box + 1)
    | Some interval -> pf ppf "%a" pp_date (next_review interval card)
  in
  let _pp_last_reviewed ppf card = pf ppf "%a" pp_content card in

  if List.length cards > 0 then
    cards
    |> List.sort ~compare:(fun a b -> if a.archived then 1 else a.box - b.box)
    |> List.iter ~f:(fun card ->
           if card.archived then
             pf ppf "%a %s@." (styled `Red string) "[archived]" (title card)
           else
             pf ppf "%a %s@."
               (styled `Faint string)
               (Card.Id.to_short card.id) (title card))
  else
    Fmt.pf ppf "No card.\n"

let list_boxes () =
  let store = Store.load () in

  let print_box box_id box =
    let interval = Box.(box.interval) in
    let cards =
      List.filter
        ~f:(fun card ->
          card.box = box_id && String.(card.deck = store.current_deck))
        store.cards
    in

    let pp_box_id ppf box_id =
      Fmt.pf ppf "%a" Fmt.(styled `Green string) ("#" ^ Int.to_string box_id)
    in

    let pp_heading ppf box_id =
      let line_before = if box_id = 0 then "" else "\n" in
      Fmt.pf ppf "%s%a Every %s\n" line_before pp_box_id box_id
        (Interval.to_string interval)
    in

    Fmt.pr "%a" pp_heading box_id;
    Fmt.pr "%a" (pp_cards ~interval) cards
  in
  List.iteri ~f:print_box (Store.get_boxes store)

let list_decks () =
  let store = Store.load () in
  match store.decks with
  | [] -> Fmt.pr "No decks@."
  | _ ->
      let print_deck deck =
        if String.(store.current_deck = deck) then
          Fmt.pr "* %s\n" deck
        else
          Fmt.pr "  %s\n" deck
      in
      List.iter store.decks ~f:(fun deck -> print_deck deck.id)

let add_deck name =
  Store.load ()
  |> Store.add_deck (Deck.create ~id:name ~decks:[] ())
  |> Store.save

let rec use_deck ~input_char name =
  let store = Store.load () in
  match List.find ~f:(fun deck -> String.(deck.id = name)) store.decks with
  | Some _ ->
      Store.save { store with current_deck = name };
      Fmt.pr "Using deck %s@." name
  | None -> (
      Fmt.pr "Deck %s doesn't exist. Do you want to create it? [y/N] %!" name;
      match input_char () with
      | Some c when Char.(c = 'y' || c = 'Y') ->
          let store = store |> Store.add_deck (Deck.create ~id:name ()) in
          Store.save store;
          Fmt.pr "Deck created.@.";
          use_deck ~input_char name
      | _ -> Fmt.pr "Aborted!@." )

let show_file_content ?(with_editor = false) path =
  let filetype = Caml.Filename.extension path in
  let plainTextCard =
    List.exists [ ".md"; ".txt"; "" ] ~f:(fun extension ->
        String.(extension = filetype))
  in
  let plainTextApp =
    if plainTextCard || with_editor then
      match Caml.Sys.getenv_opt "VISUAL" with
      | Some x -> [ x ]
      | None -> (
          match Caml.Sys.getenv_opt "EDITOR" with
          | Some x -> [ x ]
          | None -> (
              match Caml.Sys.getenv_opt "PAGER" with
              | Some x -> [ x ]
              | None -> [] ) )
    else
      []
  in
  let candidates =
    if with_editor then
      plainTextApp
    else
      plainTextApp @ [ "open"; "xdg-open" ]
  in
  List.exists
    ~f:(fun bin ->
      Caml.Sys.command (bin ^ " " ^ Caml.Filename.quote path ^ " 2> /dev/null")
      <> 127)
    candidates
  |> ignore

let show_card ?(with_editor = false) id =
  let store = Store.load () in
  let card = Store.find_card_exn id store in
  match card.content with
  | Plain text -> Fmt.pr "%s\n" text
  | File (_, path) -> show_file_content ~with_editor path

(* let edit open_in_editor card_id = *)
(* let store = Store.load () in *)
(* let card = Store.find_card_exn card_id store in *)
(* let content = match card.content with Plain text | File (_, text) -> text in *)
(* let new_content = open_in_editor (content ^ Editor.default_template) in *)
(* let new_id = Card.Id.generate new_content in *)
(* let new_card = { card with content = Plain new_content; id = new_id } in *)
(* store |> Store.set_card card.id new_card |> Store.save; *)
(*  *)
(* if String.(new_id = card.id) then *)
(* Fmt.pr "Edited card %a@." Console.yellow_s @@ new_card.id *)
(* else *)
(* Fmt.pr "Edited card %a (new name %a)@." Console.yellow_s card.id *)
(* Console.green_s *)
(* @@ new_card.id *)

let remove input_char card_id =
  let store = Store.load () in
  let card = Store.find_card_exn card_id store in
  Fmt.pr "You are about to remove the card '%a', continue? [y/N]: %!"
    Console.magenta_s
  @@ Card.title card;
  match input_char () with
  | Some c when Char.(c = 'y' || c = 'Y') ->
      let cards =
        List.filter store.cards ~f:(fun c -> not String.(c.id = card.id))
      in
      Store.save { store with cards };
      Fmt.pr "Card removed.@."
  | _ -> Fmt.pr "Aborted!@."

let archive card_id =
  let store = Store.load () in
  let card = Store.find_card_exn card_id store in
  Store.set_card card.id { card with archived = true } store |> Store.save;
  Fmt.pr "Card Archived.@."

let unarchive card_id =
  let store = Store.load () in
  let card = Store.find_card_exn card_id store in
  Store.set_card card.id { card with archived = false } store |> Store.save;
  Fmt.pr "Card unarchived.@."

let move_card ~at card_id box_id =
  let store = Store.load () in
  Store.move_card_to at (box_id - 1) card_id store |> Store.save

let content_arg =
  Arg.(
    info [ "c"; "content" ] ~docv:"CONTENT"
      ~doc:"The content in text of the card"
    |> opt (some string) None
    |> value)

let add_deck_cmd =
  let name_arg =
    Arg.(
      info [] ~docv:"name" ~doc:"deck name"
      |> pos ~rev:true 0 (some string) None
      |> required)
  in
  let action = Term.(const add_deck $ name_arg) in
  let info = Term.info "add-deck" in
  (action, info)

let complete_ids () =
  let store = Store.load () in
  let cards = store.cards in
  List.iter cards ~f:(fun card -> Fmt.pr "%s " Card.(card.id));
  Fmt.pr "@."

let zshids () =
  let store = Store.load () in
  let cards = Store.get_cards store in
  List.iter cards ~f:(fun card -> Fmt.pr "%s:%s\n" card.id (Card.title card))

let card_id_arg =
  Arg.(
    info [] ~docv:"ID" ~doc:"Id of the card"
    |> pos ~rev:true 0 (some string) None
    |> required)

let rate ~at (rating : Card.Rating.t) card_id =
  let open Card.Rating in
  let store = Store.load () in
  let card = Store.find_card_exn card_id store in
  let move_card_to = Store.move_card_to at in
  begin
    match rating with
    | Bad -> store |> move_card_to 0 card_id |> Store.save
    | Again -> store |> move_card_to card.box card_id |> Store.save
    | Good -> store |> move_card_to (card.box + 1) card_id |> Store.save
    | Easy ->
        store
        |> move_card_to (List.length (Store.get_boxes store) - 1) card_id
        |> Store.save
  end;
  Fmt.pr "Card rated %a\n" Console.magenta_s
  @@ String.lowercase
  @@ Card.Rating.to_string rating

let review ?(deck = None) now =
  let open Box in
  let store = Store.load () in
  let deck = match deck with Some deck -> deck | None -> store.current_deck in

  let _should_review (card : Card.t) =
    if String.(card.deck = deck) then
      let box = List.nth_exn (Store.get_boxes ~deck store) card.box in
      Float.(next_review box.interval card <= now)
    else
      false
  in
  store |> Store.get_cards ~deck
  |> List.filter ~f:(fun card -> not Card.(card.archived))
  |> print_cards_to_review now store
