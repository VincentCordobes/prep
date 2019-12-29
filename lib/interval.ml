open Base
open Yojson.Safe.Util

type t = 
  | Day of int 
  | Week of int 
[@@deriving show]

let of_yojson interval = 
  let value =
    interval |> member "value" |> to_int 
  in
  match interval |> member "unit" |> to_string with
  | "day" -> Ok (Day value)
  | "week" -> Ok (Week value)
  | _ -> Error "Invalid interval" 


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
  | Week m, Day n -> m = n
  | _ -> false 


let of_string str = 
  let regex = Str.regexp {|\([0-9]+\)\(d\|w\)$|} in
  match Str.string_match regex str 0 with
  | true -> (
      let n = Int.of_string (Str.matched_group 1 str) in
      let unit_str = Str.matched_group 2 str in
      match unit_str with
      | "d" | "days" | "day" -> `Ok (Day n)
      | "w" | "weeks" | "week" -> `Ok (Week n)
      | _ -> failwith (unit_str ^ " is not a valid unit") )
  | false -> `Error (str ^ " is not a valid interval")
