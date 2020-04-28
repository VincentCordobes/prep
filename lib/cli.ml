open Base
open Cmdliner

let rec add ?(last_reviewed_at = Unix.time ()) ?(retry = false) content =
  let content =
    match content with
    | Some s -> if retry then Editor.edit (s ^ Editor.default_template) else s
    | None -> Editor.edit Editor.default_template
  in
  let store = Store.load () in
  let id = Card.generate_id content in
  let exists = Store.exists ~exact:true id store in
  if exists then (
    Fmt.pr "This name already exists. Press any key to continue...@.";
    Caml.(input_char Caml.stdin) |> ignore;
    add (Some content) ~retry:true )
  else
    match Card.create id (Plain content) last_reviewed_at with
    | Ok card ->
        let updated_store = Store.add card store in
        Store.save updated_store;
        Fmt.pr "Card added (id: %s)\n" card.id
    | Error msg -> failwith msg

let add_file name ?(last_reviewed_at = Unix.time ()) file =
  let open Caml in
  let store = Store.load () in
  let id =
    Card.generate_id
      (match name with None -> Filename.basename file | Some s -> s)
  in
  let exists = Store.exists ~exact:true id store in
  if exists then
    Fmt.pr "This path already exists@."
  else
    let path =
      if Filename.is_relative file then
        Filename.concat (Sys.getcwd ()) file
      else
        file
    in
    match
      Card.create id ~deck:store.current_deck
        (File (name, path))
        last_reviewed_at
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

let print_cards ?interval cards =
  let open Card in
  let open ISO8601.Permissive in
  let open Fmt in
  let grey = styled `Faint string in

  let pp_content ppf card =
    match interval with
    | None -> pf ppf "%a #%d" grey "box" (card.box + 1)
    | Some interval ->
        pf ppf "%a %a%a %a" grey "last" pp_date card.last_reviewed_at grey
          ", next" pp_date
          (next_review interval card)
  in
  let pp_last_reviewed ppf card =
    pf ppf "%a%a%a" grey "(" pp_content card grey ")"
  in
  if List.length cards > 0 then
    List.iter cards ~f:(fun card ->
        pr "* %a %a@." Console.yellow_s (title card) pp_last_reviewed card)
  else
    Fmt.pr "No card.\n"

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
    Fmt.pr "Every %a\n" Console.green_s (Interval.to_string interval);
    print_cards ~interval cards
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

let add_deck name parent =
  Store.load ()
  |> Store.add_deck (Deck.create ~id:name ~deck_id:parent ())
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

let show_file_content path =
  let filetype = Caml.Filename.extension path in
  let can_open_with_editor =
    List.exists [ ".md"; ".txt"; "" ] ~f:(fun extension ->
        String.(extension = filetype))
  in
  let candidates =
    if can_open_with_editor then
      match Caml.Sys.getenv_opt "EDITOR" with Some x -> [ x ] | None -> []
    else
      []
  in
  let candidates = candidates @ [ "open"; "xdg-open" ] in
  List.exists
    ~f:(fun bin ->
      Caml.Sys.command (bin ^ " " ^ Caml.Filename.quote path ^ " 2> /dev/null")
      <> 127)
    candidates
  |> ignore

let show_card id =
  let store = Store.load () in
  let card = Store.find_card_exn id store in
  match card.content with
  | Plain text -> Fmt.pr "%s\n" text
  | File (_, path) -> show_file_content path

let edit open_in_editor card_id =
  let store = Store.load () in
  let card = Store.find_card_exn card_id store in
  let content = match card.content with Plain text | File (_, text) -> text in
  let new_content = open_in_editor (content ^ Editor.default_template) in
  let new_id = Card.generate_id new_content in
  let new_card = { card with content = Plain new_content; id = new_id } in
  store |> Store.set_card card.id new_card |> Store.save;

  if String.(new_id = card.id) then
    Fmt.pr "Edited card %a@." Console.yellow_s @@ new_card.id
  else
    Fmt.pr "Edited card %a (new name %a)@." Console.yellow_s card.id
      Console.green_s
    @@ new_card.id

let remove input_char card_id =
  let store = Store.load () in
  let card = Store.find_card_exn card_id store in
  Fmt.pr "You are about to remove the card %a, continue? [y/N]: %!"
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

let move_card ~at card_id box_id =
  let store = Store.load () in
  Store.move_card_to at (box_id - 1) card_id store |> Store.save

let move_down ~at card_id =
  let store = Store.load () in
  let card = Store.find_card_exn card_id store in
  Store.move_card_to at (card.box - 1) card_id store |> Store.save

let content_arg =
  Arg.(
    info [ "c"; "content" ] ~docv:"CONTENT"
      ~doc:"The content in text of the card"
    |> opt (some string) None
    |> value)

let add_cmd =
  let now = Unix.time () in
  let action =
    Term.(const (add ~last_reviewed_at:now ~retry:false) $ content_arg)
  in
  let info = Term.info "add" ~exits:Term.default_exits in
  (action, info)

let add_file_cmd =
  let now = Unix.time () in
  let path_arg =
    Arg.(
      info [] ~docv:"PATH" ~doc:"path of the card"
      |> pos ~rev:true 0 (some file) None
      |> required)
  in

  let name_arg =
    Arg.(
      info [ "n"; "name" ] ~docv:"NAME" ~doc:"Name of the card"
      |> opt (some string) None
      |> value)
  in
  let action =
    Term.(const (add_file ~last_reviewed_at:now) $ name_arg $ path_arg)
  in
  let info = Term.info "add-file" ~exits:Term.default_exits in
  (action, info)

let add_box_cmd =
  let interval = (Interval.of_string, fun ppf -> Fmt.pf ppf "%a" Interval.pp) in
  let interval_arg =
    Arg.(
      info [ "f"; "interval" ] ~docv:"interval"
        ~doc:"interval in days or weeks of the box"
      |> opt (some interval) None
      |> required)
  in
  let action = Term.(const add_box $ interval_arg) in
  let info = Term.info "add-box" in
  (action, info)

let add_deck_cmd =
  let name_arg =
    Arg.(
      info [] ~docv:"name" ~doc:"deck name"
      |> pos ~rev:true 0 (some string) None
      |> required)
  in
  let parent_arg =
    Arg.(
      info [ "p"; "parent" ] ~docv:"parent" ~doc:"parent deck"
      |> opt (some string) None
      |> value)
  in
  let action = Term.(const add_deck $ name_arg $ parent_arg) in
  let info = Term.info "add-deck" in
  (action, info)

let list_boxes_cmd = (Term.(const list_boxes $ const ()), Term.info "list-boxes")

let list_decks_cmd = (Term.(const list_decks $ const ()), Term.info "decks")

let complete_ids () =
  let store = Store.load () in
  let cards = store.cards in
  List.iter cards ~f:(fun card -> Fmt.pr "%s " Card.(card.id));
  Fmt.pr "@."

let complete_ids_cmd =
  let action = Term.(const complete_ids $ const ()) in
  let info = Term.info "complete-ids" in
  (action, info)

let card_id_arg =
  Arg.(
    info [] ~docv:"ID" ~doc:"Id of the card"
    |> pos ~rev:true 0 (some string) None
    |> required)

let show_card_cmd = (Term.(const show_card $ card_id_arg), Term.info "show")

let edit_cmd = (Term.(const (edit Editor.edit) $ card_id_arg), Term.info "edit")

let remove_cmd =
  let input_char () = Stdio.(In_channel.input_char Caml.stdin) in
  let action = Term.(const (remove input_char) $ card_id_arg) in
  let info = Term.info "remove" in
  (action, info)

let move_card_cmd =
  let now = Unix.time () in
  let box_id_arg =
    Arg.(
      info [] ~docv:"BOX_ID" ~doc:"Id of the box"
      |> pos ~rev:true 0 (some int) None
      |> required)
  in
  (Term.(const (move_card ~at:now) $ card_id_arg $ box_id_arg), Term.info "move")

let move_down_cmd =
  let now = Unix.time () in
  (Term.(const (move_down ~at:now) $ card_id_arg), Term.info "move-down")

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

let rate_cmd =
  let rating =
    let parse value =
      match Card.Rating.of_string value with
      | Ok r -> `Ok r
      | Error msg -> `Error msg
    in
    let pp ppf = Fmt.pf ppf "%a" Card.Rating.pp in
    (parse, pp)
  in
  let rating_arg =
    Arg.(
      info [] ~docv:"RATING" ~doc:"Rating value"
      |> pos 0 (some rating) None
      |> required)
  in
  let now = Unix.time () in
  let action = Term.(const (rate ~at:now) $ rating_arg $ card_id_arg) in
  let info = Term.info "rate" in
  (action, info)

let review now =
  let open Box in
  let store = Store.load () in
  let should_review (card : Card.t) =
    let box = List.nth_exn (Store.get_boxes store) card.box in
    Float.(next_review box.interval card <= now)
  in
  List.filter store.cards ~f:should_review |> print_cards

let review_cmd =
  let now = Unix.time () in
  (Term.(const review $ const now), Term.info "review")

let use_deck_cmd =
  let name_arg =
    Arg.(
      info [] ~docv:"name" ~doc:"deck name"
      |> pos ~rev:true 0 (some string) None
      |> required)
  in
  let input_char () = Stdio.(In_channel.input_char Caml.stdin) in
  let action = Term.(const (use_deck ~input_char) $ name_arg) in
  let info = Term.info "use-deck" in
  (action, info)
