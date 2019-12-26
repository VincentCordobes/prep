open Base
open Core
open Stdio
open Cmdliner


(* TODO: make type for shor_id and id *)

(* scheduled review *)
(* newly introduced and difficult cards are shown more frequently while older or less diffcult are shown less frequently *)
module Card = struct
  type t = {
    id: string; [@printer fun fmt -> fprintf fmt "%s"]
    content: string;
  } [@@deriving show, yojson]

  let short_id card_id = (Str.first_chars card_id 7)

  let title card = String.split_lines card.content |> List.hd_exn
end


module Frequency = struct
  open Yojson.Safe.Util

  type t = Day of int | Week of int [@@deriving show]


  let of_yojson frequency = 
    let value =
      frequency |> member "value" |> to_int 
    in
    match frequency |> member "unit" |> to_string with
    | "day" -> Ok (Day value)
    | "week" -> Ok (Week value)
    | _ -> Error "Invalid frequency" 


  let to_yojson = function
    | Day value -> `Assoc [("value", `Int value); 
                           ("unit", `String "day")]
    | Week value -> `Assoc [("value", `Int value); 
                            ("unit", `String "week")]
  let to_string freqency =
    match freqency with
    | Day d -> Int.to_string d ^ if (d > 1) then  " days" else " day"
    | Week d -> Int.to_string d ^ if (d > 1) then " weeks" else " week"

  let compare a b =
    let to_days = function
      | Day n -> n 
      | Week n -> n * 7 in
    compare_int (to_days a) (to_days b)

  let equals a b =
    match a, b with
    | Day m, Day n -> m = n
    | Week m, Week n -> m = n
    | _ -> false 

end

module Box = struct
  type t = {
    frequency: Frequency.t;
    cards: Card.t list;
  } [@@deriving show, yojson]


  let create ?(cards = []) frequency  =
    {frequency; cards}

  let add card box = 
    {box with cards = card :: box.cards}

  let set card_id value box =
    let cards =
      List.map box.cards ~f:(fun card ->
          if String.(card.id = card_id) then value else card)
    in
    {box with cards}

  let remove card_id box =
    let cards = 
      List.filter box.cards ~f:(fun f -> 
          not String.(f.id = card_id)) 
    in
    {box with cards}

  let equals a b =
    Frequency.equals a.frequency b.frequency
end

let card_not_found card_id = 
  fprintf stderr "No card found with id %s\n" card_id


module Db = struct
  (** sorted *)
  type t = {boxes: Box.t list} 
  [@@deriving show, yojson {exn = true}]


  let add_box box sp =
    {
      boxes =
        List.sort (box :: sp.boxes) ~compare:(fun a b ->
            Frequency.compare a.frequency b.frequency);
    }

  let add card ?(at = 0) sp =
    let rec add i boxes =
      match boxes with
      | [] -> [Box.add card (Box.create (Frequency.Day 1))]
      | boxe :: boxes ->
        if i = at then Box.add card boxe :: boxes
        else boxe :: add (i - 1) boxes
    in
    let boxes = add 0 sp.boxes in
    {boxes}

  let load () = 
    let json_value = Yojson.Safe.from_file "db.json" in
    let boxes = of_yojson_exn json_value in
    boxes

  let save sp =
    let boxes_json = to_yojson sp in
    (* Yojson.Safe.pretty_to_channel stdout boxes_json; *)
    Yojson.Safe.to_file "db.json" boxes_json


  let all_cards sp =
    List.bind sp.boxes ~f:(fun box -> box.cards)


  let find_card_exn card_id sp =
    let box_cards =  
      List.foldi
        sp.boxes
        ~init:(Hashtbl.Poly.create ()) 
        ~f:(fun i table box -> 
            List.iter box.cards ~f:(fun card -> 
                Hashtbl.add table ~key:card.id ~data:(i, card) |> ignore
              ) ;
            table) in
   Hashtbl.find_exn box_cards card_id

  let find_card card_id sp =
    try
      let result = find_card_exn card_id sp in
      Some result
    with Not_found_s _ -> None


  let move_card_to to_box card_id sp =
    let boxes_count = List.length sp.boxes in
    if to_box >= boxes_count then
      sp
    else
      let from_box, card = find_card_exn card_id sp in
      let boxes =
        List.mapi sp.boxes ~f:(fun i box ->
            if i = from_box then Box.remove card_id box
            else if i = to_box then Box.add card box
            else box)
      in

      {boxes}
end


let content_arg =
  Arg.(
    info ["c"; "content"] ~docv:"CONTENT"
      ~doc:"The content in text of the card"
    |> opt (some string) None
    |> value
  )


let editor_template = 
      {|

# Please enter the content here. Lines starting with 
# '#' will be ignored, and so does an empty content.|}

let edit_in_editor text =
  let temp_file, outc = Filename.open_temp_file "editor" "" in

  Out_channel.fprintf outc "%s" text;
  Out_channel.close outc;

  let candidates =
    match Caml.Sys.getenv_opt "VISUAL" with
    | Some x -> [x]
    | None -> (
        match Caml.Sys.getenv_opt "EDITOR" with
        | Some x -> [x]
        | None -> (
            match Caml.Sys.getenv_opt "PAGER" with Some x -> [x] | None -> [] ) )
  in
  let is_vim =
    List.exists candidates ~f:(fun candidate ->
        List.mem ["vim"; "nvim"] candidate ~equal:equal_string)
  in
  let candidates = candidates @ ["xdg-open"; "open"] in
  let opts = if is_vim then "-c \"set filetype=gitcommit\"" else "" in
  List.exists
    ~f:(fun bin ->
        Sys.command (Filename.quote bin ^ " " ^ temp_file ^ " " ^ opts) <> 127)
    candidates
  |> ignore;
  let content = In_channel.read_all temp_file in
  let comments_regex = Str.regexp {|^#.*|} in
  let content = Str.global_replace comments_regex "" content |> String.strip in
  Caml.Sys.remove temp_file;
  content


let rec generate_id db =
  let state = Caml.Random.State.make_self_init () in
  let id = 
    Uuidm.(v4_gen state () |> to_string |> Card.short_id)
  in
  match Db.find_card id db with
  | Some _ -> generate_id db
  | None -> id


let add content =
  let content =
    match content with 
    | Some s -> s 
    | None -> edit_in_editor editor_template
  in
  let db = Db.load () in
  let id = generate_id db in
  let card : Card.t = {id; content} in
  try
    let updated_db = Db.add card db in
    Db.save updated_db;
    printf "card added (%s)" card.id
  with Failure msg -> fprintf stderr "%s\n" msg


let add_cmd = 
  Term.(const add $ content_arg), Term.info "add"

let add_box frequency =
  let db = Db.load () in
  Db.save
    (Db.add_box {frequency; cards = []} db)

let add_box_cmd =
  let parse_frequency str =
    let regex = Str.regexp {|\([0-9]+\)\(d\|w\)|} in
    match Str.string_match regex str 0 with
    | true -> (
        let n = int_of_string (Str.matched_group 1 str) in
        let unit_str = Str.matched_group 2 str in
        match unit_str with
        | "d" | "days" | "day" -> `Ok (Frequency.Day n)
        | "w" | "weeks" | "week" -> `Ok (Frequency.Week n)
        | _ -> failwith (unit_str ^ " is not a valid unit") )
    | false -> `Error (str ^ " is not a valid frequency")
  in
  let frequency : Frequency.t Arg.conv =
    ( parse_frequency,
      fun ppf f -> Caml.Format.fprintf ppf "%s" (Frequency.show f) )
  in
  let frequency_arg =
    Arg.(
      info ["f"; "frequency"] ~docv:"FREQUENCY"
        ~doc:"frequency in days or weeks of the box"
      |> opt (some frequency) None
      |> required)
  in
  (Term.(const add_box $ frequency_arg), Term.info "add-box")

let list_boxes () =
  let db = Db.load () in
  List.iter
    ~f:(fun {frequency; cards} ->
        printf "Every %s\n" (Frequency.to_string frequency);
        if List.length cards > 0 then (
          List.iter
            ~f:(fun card ->
                printf "* %s %s\n" card.id (Card.title card))
            cards;
          printf "\n" ))
    db.boxes


let list_boxes_cmd =
  Term.(const list_boxes $ const ()), Term.info "list-boxes"


let card_id_arg =
  Arg.(
    info [] ~docv:"ID" ~doc:"Id of the card"
    |> pos 0 (some string) None
    |> required
  )


let edit card_id =
  let db = Db.load () in
  let box_id, card = Db.find_card_exn card_id db in
  let new_content = edit_in_editor (card.content ^ editor_template) in
  Db.save
    {
      boxes =
        List.mapi db.boxes ~f:(fun i box ->
            if i = box_id then
              let new_card = {card with content = new_content} in
              Box.set card_id new_card box
            else box);
    }
    

let edit_cmd = Term.(const edit $ card_id_arg), Term.info "edit"


let remove id =
  let db = Db.load () in
  let all_cards = Db.all_cards db in
  let matching_cards =
    List.filter all_cards ~f:(fun card ->
        String.(card.id = id))
  in
  match matching_cards with
  | [] -> card_not_found id
  | [card] ->
    let sp : Db.t =
      {boxes = List.map db.boxes ~f:(fun boxe -> Box.remove card.id boxe)}
    in
    Db.save sp;
    printf "card removed\n"
  | _ -> fprintf stderr "Multiple card with the same short id.\n" 

let remove_cmd = Term.(const remove $ card_id_arg), Term.info "remove"


let move_card card_id box_id =
  let db = Db.load () in
  try 
    Db.move_card_to box_id card_id db
    |> Db.save
  with
  | Not_found_s _ -> card_not_found card_id

let move_card_cmd =
  let box_id_arg =
    Arg.(
      info [] ~docv:"BOX_ID" ~doc:"Id of the box"
      |> pos ~rev:true 0 (some int) None
      |> required
    ) in
  (Term.(const move_card $ card_id_arg $ box_id_arg), Term.info "move")


let move_up card_id =
  let db = Db.load () in
  let box_id, _ = Db.find_card_exn card_id db in
  Db.move_card_to (box_id + 1) card_id db
  |> Db.save


let move_up_cmd =
  (Term.(const move_up $ card_id_arg), Term.info "move-up")

let () =
  Term.eval_choice add_cmd
    [
      list_boxes_cmd;
      add_cmd;
      add_box_cmd;
      edit_cmd;
      remove_cmd;
      move_card_cmd;
      move_up_cmd;
    ]
  |> Term.exit
