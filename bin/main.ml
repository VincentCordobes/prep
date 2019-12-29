open Cmdliner
open Rehearsal.Cli

let default_cmd =
  let doc = "A spaced-repetition tool" in
  let sdocs = Manpage.s_common_options in
  let exits = Term.default_exits in
  Term.(ret (const (fun _ -> `Help (`Pager, None)) $ const ())),
  Term.info "rehearsal" ~version:"v1.0" ~doc ~sdocs ~exits 

let () =
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  Term.eval_choice default_cmd
    [
      list_boxes_cmd;
      show_card_cmd;
      add_cmd;
      add_box_cmd;
      edit_cmd;
      remove_cmd;
      move_card_cmd;
      move_up_cmd;
      move_down_cmd;
    ]
  |> Term.exit
