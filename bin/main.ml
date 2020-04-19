open Cmdliner
open Prep.Cli

let default_cmd =
  let doc = "A spaced-repetition tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  ( Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
    Term.info "prep" ~version:"v1.0" ~doc ~sdocs ~exits )

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
        edit_cmd;
        remove_cmd;
        move_card_cmd;
        move_down_cmd;
        rate_cmd;
        review_cmd;
        (* interactive_cmd *)
      ]
    |> Term.exit
  with _ -> exit 1
