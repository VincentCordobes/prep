open Base
open Core
open Stdio
open Cmdliner


(* TODO: make type for shor_id and id *)

(* scheduled review *)
(* newly introduced and difficult flashcards are shown more frequently while older or less diffcult are shown less frequently *)
module FlashCard = struct
  type t = {
    id: string; [@printer fun fmt -> fprintf fmt "%s"]
    content: string;
  } [@@deriving show, yojson]

  let short_id flashcard_id = (Str.first_chars flashcard_id 7)

  let title flashcard = String.split_lines flashcard.content |> List.hd_exn
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
    flashcards: FlashCard.t list;
  } [@@deriving show, yojson]


  let create ?(flashcards = []) frequency  =
    {frequency; flashcards}

  let add flashcard box = 
    {box with flashcards = flashcard :: box.flashcards}

  let remove flashcard_id box =
    {
      box with
      flashcards =
        List.filter box.flashcards ~f:(fun f ->
            not String.(f.id = flashcard_id));
    }

  let equals a b =
    Frequency.equals a.frequency b.frequency
end

let flashcard_not_found flashcard_id = 
  fprintf stderr "No flashcard found with id %s\n" flashcard_id


module Spaced_repetition = struct
  (** sorted *)
  type t = {boxes: Box.t list} 
  [@@deriving show, yojson {exn = true}]


  let add_box box sp =
    {
      boxes =
        List.sort (box :: sp.boxes) ~compare:(fun a b ->
            Frequency.compare a.frequency b.frequency);
    }

  let add flashcard ?(at = 0) sp =
    let rec add i boxes =
      match boxes with
      | [] -> [Box.add flashcard (Box.create (Frequency.Day 1))]
      | boxe :: boxes ->
        if i = at then Box.add flashcard boxe :: boxes
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


  let all_flashcards sp =
    List.bind sp.boxes ~f:(fun box -> box.flashcards)


  let find_flashcard_exn flashcard_id sp =
    let box_flashcards =  
      List.foldi
        sp.boxes
        ~init:(Hashtbl.Poly.create ()) 
        ~f:(fun i table box -> 
            List.iter box.flashcards ~f:(fun flashcard -> 
                Hashtbl.add table ~key:flashcard.id ~data:(i, flashcard) |> ignore
              ) ;
            table) in
   Hashtbl.find_exn box_flashcards flashcard_id

  let find_flashcard flashcard_id sp =
    try
      let result = find_flashcard_exn flashcard_id sp in
      Some result
    with Not_found_s _ -> None


  let move_card_to to_box flashcard_id sp =
    let from_box, flashcard = find_flashcard_exn flashcard_id sp in
    let boxes =
      List.mapi sp.boxes ~f:(fun i box ->
          if i = from_box then Box.remove flashcard_id box
          else if i = to_box then Box.add flashcard box
          else box)
    in

    {boxes}
end






let content_arg =
  Arg.(
    info ["c"; "content"] ~docv:"CONTENT"
      ~doc:"The content in text of the flashcard"
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
    | Some x -> [ x ]
    | None -> (
        match Caml.Sys.getenv_opt "EDITOR" with
        | Some x -> [ x ]
        | None -> (
            match Caml.Sys.getenv_opt "PAGER" with
            | Some x -> [ x ]
            | None -> [] ) )
  in
  let candidates = candidates @ [ "xdg-open"; "open" ] in
  (List.exists
     ~f:(fun bin ->
         Sys.command (Filename.quote bin ^ " " ^ temp_file) <> 127)
     candidates) |> ignore;
  let content = In_channel.read_all temp_file in
  let comments_regex = Str.regexp {|^#.*|} in
  let content = 
    Str.global_replace comments_regex "" content 
    |> String.strip 
  in
  Caml.Sys.remove temp_file;
  content


let rec generate_id db =
  let state = Caml.Random.State.make_self_init () in
  let id = 
    Uuidm.(v4_gen state () |> to_string |> FlashCard.short_id)
  in
  match Spaced_repetition.find_flashcard id db with
  | Some _ -> generate_id db
  | None -> id


let add content =
  let content =
    match content with 
    | Some s -> s 
    | None -> edit_in_editor editor_template
  in
  let db = Spaced_repetition.load () in
  let id = generate_id db in
  let flashcard : FlashCard.t = {id; content} in
  try
    let updated_db = Spaced_repetition.add flashcard db in
    Spaced_repetition.save updated_db;
    printf "Flashcard added (%s)" flashcard.id
  with Failure msg -> fprintf stderr "%s\n" msg


let add_cmd = 
  Term.(const add $ content_arg), Term.info "add"


let list_boxes () =
  let db = Spaced_repetition.load () in
  List.iter
    ~f:(fun {frequency; flashcards} ->
        printf "Every %s\n" (Frequency.to_string frequency);
        if List.length flashcards > 0 then (
          List.iter
            ~f:(fun flashcard ->
                printf "* %s %s\n" flashcard.id (FlashCard.title flashcard))
            flashcards;
          printf "\n" ))
    db.boxes


let list_boxes_cmd =
  Term.(const list_boxes $ const ()), Term.info "list-boxes"

let edit () =
  let content =
    edit_in_editor
      {|

# Please enter the content here. Lines starting with 
# '#' will be ignored, and so does an empty content.|}
  in
  printf "What you typed is:\n%s\n" content


let edit_cmd = Term.(const edit $ const ()), Term.info "edit"




let remove id =
  let db = Spaced_repetition.load () in
  let all_flashcards = Spaced_repetition.all_flashcards db in
  let matching_flashcards =
    List.filter all_flashcards ~f:(fun flashcard ->
        String.(flashcard.id = id))
  in
  match matching_flashcards with
  | [] -> flashcard_not_found id
  | [flashcard] ->
    let sp : Spaced_repetition.t =
      {boxes = List.map db.boxes ~f:(fun boxe -> Box.remove flashcard.id boxe)}
    in
    Spaced_repetition.save sp;
    printf "Flashcard removed\n"
  | _ -> fprintf stderr "Multiple flashcard with the same short id.\n" 


let flashcard_id_arg =
  Arg.(
    info [] ~docv:"ID" ~doc:"Id of the flashcard"
    |> pos 0 (some string) None
    |> required
  )



let remove_cmd = Term.(const remove $ flashcard_id_arg), Term.info "remove"


let add_box frequency =
  let db = Spaced_repetition.load () in
  Spaced_repetition.save
    (Spaced_repetition.add_box {frequency; flashcards = []} db)


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

let move_card flashcard_id box_id =
  let db = Spaced_repetition.load () in
  try 
    Spaced_repetition.move_card_to box_id flashcard_id db
    |> Spaced_repetition.save
  with
  | Not_found_s _ -> flashcard_not_found flashcard_id


let move_card_cmd =
  let box_id_arg =
    Arg.(
      info [] ~docv:"BOX_ID" ~doc:"Id of the box"
      |> pos ~rev:true 0 (some int) None
      |> required
    ) in
  (Term.(const move_card $ flashcard_id_arg $ box_id_arg), Term.info "move")

let () =
  Term.eval_choice add_cmd
    [add_cmd; list_boxes_cmd; edit_cmd; remove_cmd; move_card_cmd; add_box_cmd]
  |> Term.exit
