open Base
open Core
open Stdio


(* scheduled review *)
(* newly introduced and difficult flashcards are shown more frequently while older or less diffcult are shown less frequently *)
module FlashCard = struct
  type t = {
    id: string;
    content: string;
  } [@@deriving show]

  let of_json value =
    let open Yojson.Basic.Util in
    let id = value |> member "id" |> to_string in
    let content = value |> member "content" |> to_string in
    {id; content}

  let to_json (flashcard: t): Yojson.Basic.t =
    `Assoc [
      ("id", `String flashcard.id);
      ("content", `String flashcard.content)
    ]

end


module Frequency = struct
  type f_unit = [ `day | `week] [@@deriving show]

  type t = {
    value: int;
    unit: f_unit;
  } [@@deriving show]

  let of_json json_value =
    let open Yojson.Basic.Util in
    let value = json_value |> member "value" |> to_int in
    let unit = 
      match  json_value |> member "unit" |> to_string with
      | "day" -> `day
      | "week" -> `week
      | _ -> failwith "Invalid unit" 
    in
    {value; unit}


  let to_json (frequency: t): Yojson.Basic.t =
    `Assoc [
      ("value", `Int frequency.value);
      ("unit", `String "day");
    ]
end


module Box = struct
  type t = {
    frequency: Frequency.t;
    flashcards: FlashCard.t list;
  } [@@deriving show]


  let create frequency flashcards =
    {frequency=frequency; flashcards=flashcards;}

  let add flashcard box = 
    { box with flashcards = flashcard::box.flashcards }

  let remove flashcard_id box =
    { box with 
      flashcards = List.filter ~f:(fun f -> 
          not (phys_equal f.id flashcard_id)) box.flashcards }

  let of_json json_value =
    let open Yojson.Basic.Util in
    let frequency = json_value
                    |> member "frequency"
                    |> Frequency.of_json 
    in
    let flashcards = json_value
                     |> member "flashcards"
                     |> to_list
                     |> List.map ~f:FlashCard.of_json
    in
    {frequency; flashcards}


  let to_json (box: t): Yojson.Basic.t =
    `Assoc [
      ("frequency", Frequency.to_json box.frequency);
      ("flashcards", `List (List.map box.flashcards (fun x -> FlashCard.to_json x)));
    ]


end


let load_boxes () = 
  let open Yojson.Basic.Util in
  let json_value = Yojson.Basic.from_file "db.json" in
  let boxes = json_value 
              |> member "boxes" 
              |> to_list
              |> List.map ~f:(Box.of_json)
  in
  boxes

let save_boxes (boxes) =
  let boxes_json = 
    `Assoc [
      ("boxes", `List (List.map boxes Box.to_json))
    ] 
  in
  Yojson.Basic.pretty_to_channel stdout boxes_json


let _ =
  let boxes = load_boxes () in
  printf "Loading boxes...\n";
  List.iter boxes (fun box -> printf "%s" (Box.show box));
  printf "\n\n";
  printf "Saving boxes...\n";
  save_boxes boxes

