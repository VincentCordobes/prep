open Base
open Cmdliner
open Rehearsal

let content_arg =
  Arg.(
    info ["c"; "content"] ~docv:"CONTENT"
      ~doc:"The content in text of the card"
    |> opt (some string) None
    |> value
  )


let add_cmd = 
  Term.(const add $ content_arg), Term.info "add"


let add_box_cmd =
  let parse_interval str =
    let regex = Str.regexp {|\([0-9]+\)\(d\|w\)|} in
    match Str.string_match regex str 0 with
    | true -> (
        let n = Int.of_string (Str.matched_group 1 str) in
        let unit_str = Str.matched_group 2 str in
        match unit_str with
        | "d" | "days" | "day" -> `Ok (Interval.Day n)
        | "w" | "weeks" | "week" -> `Ok (Interval.Week n)
        | _ -> failwith (unit_str ^ " is not a valid unit") )
    | false -> `Error (str ^ " is not a valid interval")
  in
  let interval : Interval.t Arg.conv =
    ( parse_interval,
      fun ppf f -> Caml.Format.fprintf ppf "%s" (Interval.show f) )
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



let move_up_cmd =
  (Term.(const move_up $ card_id_arg), Term.info "move-up")


let move_down_cmd =
  (Term.(const move_down $ card_id_arg), Term.info "move-down")

(* let list_today () = *)
(*   let db = Db.load() in *)


let () =
  Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ();
  Term.eval_choice add_cmd
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
