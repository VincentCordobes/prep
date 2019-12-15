open Base
open Core
open Stdio
open Cmdliner

(* scheduled review *)
(* newly introduced and difficult flashcards are shown more frequently while older or less diffcult are shown less frequently *)
module FlashCard = struct
  type t = {
    name: string;
    file: string;
  } [@@deriving show, yojson]
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
            not String.(f.name = flashcard_id));
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
    Yojson.Safe.pretty_to_channel stdout boxes_json;
    Yojson.Safe.to_file "db.json" boxes_json
end


let path_arg =
  Arg.(
    info ["p"; "path"] ~docv:"PATH"
      ~doc:"The path to the file you want to practice on"
    |> opt (some file) None
    |> required)

let name_arg =
  Arg.(
    info ["n"; "name"] ~docv:"NAME" ~doc:"Name of the flashcard"
    |> opt (some string) None
    |> required)


let myProg path name = 
  printf "Here is the path %s %s" path name


let cmd = 
  Term.(const myProg $ path_arg $ name_arg), Term.info "spaced-repetition"


let add path name =
  let db_result = Spaced_repetition.load () in
  printf "Loading data...\n";
  match db_result with
  | Ok db ->
      let sp : Spaced_repetition.t =
        {boxes = List.map db.boxes~f:(Box.add {name; file = path})}
      in
      (* printf "%s\n\n" (Spaced_repetition.show sp); *)
      printf "Saving boxes...\n";
      Spaced_repetition.save sp
  | Error e -> fprintf stderr "Error in %s\n" e


let list_boxes =
  printf "Boxes"


let add_cmd = 
  Term.(const add $ path_arg $ name_arg), Term.info "add"

let list_cmd =
  Term.(const list_boxes), Term.info "list-boxes"


let () = 
  Term.eval_choice cmd [cmd; add_cmd]
  |> Term.exit 
