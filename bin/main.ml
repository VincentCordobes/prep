open Cmdliner
open Prep.Cli

let default_cmd =
  let doc = "A spaced-repetition tool" in
  let man =
    [
      `S Manpage.s_description;
      `P
        "$(b,Prep) is a command line utility to help learn, memorize things. \
         It is based on the spaced repetition learning technique.";
      `P
        "A $(b,card) is an item you want to learn/practice. It could be \
         anything, from a plain plain text file to a video, a PDF, a song and \
         so one.";
      `P
        "$(b,Boxes) are associated with a time interval—duration between \
         each practice session. When we successfully review a card, it \
         graduates to the next box and will be shown less often. Otherwise it \
         goes down to the previous one. The first box is well suited for newly \
         added cards or cards that are hard to grasp while the last box \
         contains cards we already master.";
      `P
        "Finally, $(b,deck) groups related cards together. It can also group \
         other decks in a tree-like structure.";
    ]
  in
  let sdocs = Manpage.s_common_options in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
    Term.info "prep" ~version:"v1.0" ~doc ~sdocs ~man )

let add_cmd =
  let now = Unix.time () in
  let action =
    Term.(const (add ~last_reviewed_at:now ~retry:false) $ content_arg)
  in
  let info =
    Term.info "add" ~doc:"Add a card to the first box of the current deck"
      ~sdocs:Manpage.s_common_options ~exits:Term.default_exits
  in
  (action, info)

let add_box_cmd =
  let interval =
    (Prep.Interval.of_string, fun ppf -> Fmt.pf ppf "%a" Prep.Interval.pp)
  in
  let interval_arg =
    Arg.(
      info [ "interval" ] ~docv:"interval"
        ~doc:"interval in days or weeks of the box"
      |> opt (some interval) None
      |> required)
  in
  let action = Term.(const add_box $ interval_arg) in
  let info =
    Term.info "add-box" ~doc:"Create a box in the current deck"
      ~sdocs:Manpage.s_common_options
  in
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
  let add_file name path = add_file ~last_reviewed_at:now ~name path in
  let action = Term.(const add_file $ name_arg $ path_arg) in
  let info =
    Term.info "add-file" ~doc:"Add a card of type file"
      ~sdocs:Manpage.s_common_options ~exits:Term.default_exits
  in
  (action, info)

let complete_ids_cmd =
  let action = Term.(const complete_ids $ const ()) in
  let info = Term.info "complete-ids" ~doc:"List all card ids" in
  (action, info)

let zshids_cmd =
  let action = Term.(const zshids $ const ()) in
  let info =
    Term.info "zshids" ~doc:"Shows the IDs and descriptions of matching tasks"
  in
  (action, info)

let use_deck_cmd =
  let name_arg =
    Arg.(
      info [] ~docv:"name" ~doc:"deck name"
      |> pos ~rev:true 0 (some string) None
      |> required)
  in
  let input_char () = Stdio.(In_channel.input_char Caml.stdin) in
  let action = Term.(const (use_deck ~input_char) $ name_arg) in
  let info =
    Term.info "use-deck" ~doc:"Switch the current deck"
      ~sdocs:Manpage.s_common_options
  in
  (action, info)

let list_decks_cmd =
  ( Term.(const list_decks $ const ()),
    Term.info "decks" ~doc:"List all decks" ~sdocs:Manpage.s_common_options )

(* let edit_cmd = *)
(* ( Term.(const (edit Prep.Editor.edit) $ card_id_arg), *)
(* Term.info "edit" ~doc:"Edit card details" ~sdocs:Manpage.s_common_options ) *)

let list_boxes_cmd =
  ( Term.(const list_boxes $ const ()),
    Term.info "boxes" ~doc:"List all boxes" ~sdocs:Manpage.s_common_options )

let move_card_cmd =
  let now = Unix.time () in
  let box_id_arg =
    Arg.(
      info [] ~docv:"BOX_ID" ~doc:"Id of the box"
      |> pos ~rev:true 0 (some int) None
      |> required)
  in
  ( Term.(const (move_card ~at:now) $ card_id_arg $ box_id_arg),
    Term.info "move" ~doc:"Move a card to a specific box"
      ~sdocs:Manpage.s_common_options )

let rate_cmd =
  let rating =
    let parse value =
      match Prep.Card.Rating.of_string value with
      | Ok r -> `Ok r
      | Error msg -> `Error msg
    in
    let pp ppf = Fmt.pf ppf "%a" Prep.Card.Rating.pp in
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
  let info =
    Term.info "rate" ~doc:"Rate a card" ~sdocs:Manpage.s_common_options
  in
  (action, info)

let remove_cmd =
  let input_char () = Stdio.(In_channel.input_char Caml.stdin) in
  let action = Term.(const (remove input_char) $ card_id_arg) in
  let info =
    Term.info "remove" ~doc:"Remove a card" ~sdocs:Manpage.s_common_options
  in
  (action, info)

let review_cmd =
  let now = Unix.time () in
  let deck_arg =
    Arg.info [] ~doc:"Deck name"
    |> Arg.pos ~rev:true 0 Arg.(some string) None
    |> Arg.value
  in

  let review deck = review ~deck now in
  let action = Term.(const review $ deck_arg) in
  let info =
    Term.info "review" ~doc:"List cards to be reviewed"
      ~sdocs:Manpage.s_common_options
  in
  (action, info)

let show_card_cmd =
  let with_editor_arg =
    let doc =
      "Show the card content using the editor specified by VISUAL or EDITOR \
       environment variables."
    in
    Arg.info [ "e"; "edit" ] ~doc |> Arg.flag |> Arg.value
  in

  let show_card id with_editor = show_card ~with_editor id in
  ( Term.(const show_card $ card_id_arg $ with_editor_arg),
    Term.info "show" ~doc:"Show a card" ~sdocs:Manpage.s_common_options )

let archive_card_cmd =
  let action = Term.(const archive $ card_id_arg) in
  let info =
    Term.info "archive" ~doc:"Archive a card" ~sdocs:Manpage.s_common_options
  in
  (action, info)

let unarchive_card_cmd =
  let action = Term.(const unarchive $ card_id_arg) in
  let info =
    Term.info "unarchive" ~doc:"Unarchive a card"
      ~sdocs:Manpage.s_common_options
  in
  (action, info)

let () =
  Prep.Store.init ();
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  try
    Term.eval_choice default_cmd ~catch:false
      [
        list_boxes_cmd;
        list_decks_cmd;
        use_deck_cmd;
        complete_ids_cmd;
        show_card_cmd;
        add_cmd;
        add_file_cmd;
        add_box_cmd;
        (* edit_cmd; *)
        remove_cmd;
        archive_card_cmd;
        unarchive_card_cmd;
        move_card_cmd;
        rate_cmd;
        review_cmd;
        zshids_cmd;
        (* interactive_cmd *)
      ]
    |> Term.exit
  with _ -> exit 1
