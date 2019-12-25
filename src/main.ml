open Base
open Core
open Stdio
open Cmdliner

(* scheduled review *)
(* newly introduced and difficult flashcards are shown more frequently while older or less diffcult are shown less frequently *)
module FlashCard = struct
  type t = {
    id: string; [@printer fun fmt -> fprintf fmt "%s"]
    content: string;
  } [@@deriving show, yojson]

  let short_id flashcard= (Str.first_chars flashcard.id 7)

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
end


module Box = struct
  type t = {
    frequency: Frequency.t;
    flashcards: FlashCard.t list;
  } [@@deriving show, yojson]


  let create frequency flashcards =
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

end


module Spaced_repetition = struct
  type t = {boxes: Box.t list} 
  [@@deriving show, yojson]

  let add flashcard sp =
    match sp.boxes with
    | [] -> failwith "Please add a box first"
    | box :: boxes -> {boxes = Box.add flashcard box :: boxes}

  let load () = 
    let json_value = Yojson.Safe.from_file "db.json" in
    let boxes = of_yojson json_value in
    boxes

  let save sp =
    let boxes_json = to_yojson sp in
    (* Yojson.Safe.pretty_to_channel stdout boxes_json; *)
    Yojson.Safe.to_file "db.json" boxes_json
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


let add content =
  let content = match content with
    | Some s -> s
    | None -> edit_in_editor editor_template in

  let db_result = Spaced_repetition.load () in
  (* printf "Loading data...\n"; *)
  match db_result with
  | Ok db ->
      let state = Caml.Random.State.make_self_init () in
      let id = 
        Uuidm.(v4_gen state () |> to_string)
      in
      let flashcard: FlashCard.t = {id; content} in
      let sp : Spaced_repetition.t =
        {boxes = List.map db.boxes ~f:(Box.add flashcard)}
      in
      (* printf "%s\n\n" (Spaced_repetition.show sp); *)
      Spaced_repetition.save sp;
      printf "Flashcard added (%s)" (flashcard |> FlashCard.short_id)
  | Error e -> fprintf stderr "Error in %s\n" e


let add_cmd = 
  Term.(const add $ content_arg), Term.info "add"




let list_boxes () =
  let db_result = Spaced_repetition.load () in
  match db_result with
  | Ok db ->
      List.iter
        ~f:(fun {frequency; flashcards} ->
          printf "Every %s\n" (Frequency.to_string frequency);
          List.iter
            ~f:(fun flashcard -> printf "%s\n" (flashcard.content))
            flashcards)
        db.boxes
  | Error e -> fprintf stderr "Error in %s\n" e


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

let () = 
  Term.eval_choice add_cmd [add_cmd; list_boxes_cmd; edit_cmd]
  |> Term.exit 
