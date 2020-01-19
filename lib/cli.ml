open Base
open Stdio
open Cmdliner


let exit_err msg =
  Console.print_error "%s" msg;
  Caml.exit 1

let add content =
  let content =
    match content with 
    | Some s -> s 
    | None -> Editor.edit Editor.default_template
  in
  let store = Store.load () in
  let id_exists id =
    Option.is_some @@ Store.find_card id store
  in
  let id = Card.Id.generate id_exists in
  match Card.create id content with
  | Ok card -> (
    try
      let updated_store = Store.add card store in
      Store.save updated_store;
      Fmt.pr "Card added (id: %s)\n" card.id
    with Failure msg -> exit_err msg)
  | Error msg -> exit_err msg



let add_box interval =
  let store = Store.load () in
  let box_exists =
    List.exists store.boxes ~f:(fun box ->
        Interval.compare box.interval interval = 0)
  in
  if box_exists then (
    Console.(print_error "A box with interval %a already exists" green_s)
    @@ Interval.to_string interval;
    Caml.exit 1)
  else Store.save (Store.add_box {interval; cards = []} store)


let print_cards cards =
  let open Card in 
  if List.length cards > 0 then
    List.iter cards ~f:(fun card ->
        Fmt.pr "* %a %s\n" Console.yellow_s card.id @@ title card)
  else Fmt.pr "No card.\n"


let list_boxes () =
  let store = Store.load () in
  List.iter store.boxes ~f:(fun {interval; cards} ->
      Fmt.pr "Every %s\n" (Interval.to_string interval);
      print_cards cards
    )

let show_card id =
  let store = Store.load () in
  let _, card = Store.find_card_or_exit id store in
  Fmt.pr "%s\n" card.content


let edit card_id =
  let store = Store.load () in
  let box_id, card = Store.find_card_or_exit card_id store in
  let new_content = Editor.edit (card.content ^ Editor.default_template) in
  Store.save
    {
      boxes =
        List.mapi store.boxes ~f:(fun i box ->
            if i = box_id then
              let new_card = {card with content = new_content} in
              Box.set card_id new_card box
            else box);
    };
  Fmt.pr "Edited card %a %s\n" Console.yellow_s card_id @@ Card.title card


let remove card_id =
  let store = Store.load () in
  let box_id, card = Store.find_card_or_exit card_id store in
  let sp : Store.t =
    {
      boxes =
        List.mapi store.boxes ~f:(fun i box ->
            if i = box_id then Box.remove card.id box else box);
    }
  in
  Store.save sp;
  Fmt.pr "Card removed\n"


let move_card card_id box_id =
  let store = Store.load () in
  Store.move_card_to (box_id - 1) card_id store
  |> Store.save



let move_down card_id =
  let store = Store.load () in
  let box_id, _ = Store.find_card_or_exit card_id store in
  Store.move_card_to (box_id - 1) card_id store
  |> Store.save

let content_arg =
  Arg.(
    info ["c"; "content"] ~docv:"CONTENT"
      ~doc:"The content in text of the card"
    |> opt (some string) None
    |> value
  )

let add_cmd = 
  Term.(const add $ content_arg), Term.info "add" ~exits:Term.default_exits


let add_box_cmd =
  let interval  =
    ( Interval.of_string,
      fun ppf interval -> Fmt.pf ppf "%s" (Interval.show interval) )
  in
  let interval_arg =
    Arg.(
      info ["f"; "interval"] ~docv:"interval"
        ~doc:"interval in days or weeks of the box"
      |> opt (some interval) None
      |> required)
  in
  (Term.(const add_box $ interval_arg), Term.info "add-box")



let list_boxes_cmd =
  Term.(const list_boxes $ const ()), Term.info "list-boxes"

 
let card_id_arg =
  Arg.(
    info [] ~docv:"ID" ~doc:"Id of the card"
    |> pos 0 (some string) None
    |> required
  )


let show_card_cmd =
  Term.(const show_card $ card_id_arg), Term.info "show"


let edit_cmd = Term.(const edit $ card_id_arg), Term.info "edit"


let remove_cmd = Term.(const remove $ card_id_arg), Term.info "remove"


let move_card_cmd =
  let box_id_arg =
    Arg.(
      info [] ~docv:"BOX_ID" ~doc:"Id of the box"
      |> pos ~rev:true 0 (some int) None
      |> required
    ) in
  (Term.(const move_card $ card_id_arg $ box_id_arg), Term.info "move")



let move_down_cmd =
  Term.(const move_down $ card_id_arg), Term.info "move-down"


let rate card_id rating =
  let store = Store.load () in
  let box_id, card = Store.find_card_or_exit card_id store in
  let raw_rating =
    match rating with
    | None ->
      Fmt.pr "Rate (0-3) %a: %!" Console.green_s @@ Card.title card;
      In_channel.input_line_exn stdin
    | Some value -> value
  in
  if String.(raw_rating = "") then (
    Console.print_error "invalid rating";
    Caml.exit 1 );
  match Card.Rating.from_int (Int.of_string raw_rating) with
  | Ok rating -> 
      (match rating with
      | Bad -> 
        store 
        |> Store.move_card_to 0 card_id 
        |> Store.save

      | Again -> 
        store
        |> Store.move_card_to (box_id - 1) card_id
        |> Store.save

      | Good -> 
        store
        |> Store.move_card_to (box_id + 1) card_id
        |> Store.save

      | Easy ->
        store
        |> Store.move_card_to ((List.length store.boxes) - 1) card_id
        |> Store.save);

       Fmt.pr "Your rating: %a\n" Console.magenta_s @@ Card.Rating.to_string rating
    
  | Error msg ->
    Console.print_error "%s" msg;
    Caml.exit 1

let rate_cmd =
  let rating_arg =
    Arg.(
      info [] ~docv:"RATING" ~doc:"Rating value"
      |> pos 1 (some string) None
      |> value)
  in
  (Term.(const rate $ card_id_arg $ rating_arg), Term.info "rate")


let review (now) =
  let should_review (interval : Interval.t) (card : Card.t) =
    Float.(
      let interval =
        match interval with
        | Day n -> of_int n * 24.0 * 60.0
        | Week n -> of_int n * 7.0 * 24.0 * 60.0
      in
      (card.last_reviewed_at + interval) <= now)
  in
  let store = Store.load () in
  List.bind store.boxes ~f:(fun box ->
      List.filter box.cards ~f:(should_review box.interval))
  |> print_cards


let review_cmd =
  let now = Unix.time () in
  (Term.(const review $ const now), Term.info "review")
