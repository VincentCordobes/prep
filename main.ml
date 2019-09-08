open Base
open Core
open Stdio


(* scheduled review *)
(* newly introduced and difficult flashcards are shown more frequently while older or less diffcult are shown less frequently *)
module FlashCard = struct
  type t = {
    id: string;
    content: string;
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
    { box with flashcards = flashcard::box.flashcards }

  let remove flashcard_id box =
    { box with 
      flashcards = List.filter ~f:(fun f -> 
          not (phys_equal f.id flashcard_id)) box.flashcards }
end


module Boxes = struct
  type t = {boxes: Box.t list} [@@deriving show, yojson]
end


let load_boxes () = 
  let open Yojson.Safe.Util in
  let json_value = Yojson.Safe.from_file "db.json" in
  let boxes = Boxes.of_yojson json_value in
  boxes

let save_boxes (boxes) =
  let boxes_json = Boxes.to_yojson boxes in
  Yojson.Safe.pretty_to_channel stdout boxes_json


let _ =
  let boxes_result = load_boxes () in
  printf "Loading boxes...\n";
  match boxes_result with
  | Ok boxes -> 
    printf "%s\n\n" (Boxes.show boxes);
    printf "Saving boxes...\n";
    save_boxes boxes
  | Error e -> printf "Error in %s\n" e


