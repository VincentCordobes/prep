type t = {
  id: string; [@printer fun fmt -> fprintf fmt "%s"]
  content: string;
  last_reviewed_at: float;
} [@@deriving show, yojson]

module Id = struct
  type t = string

  let rec generate id_exists =
    let short_id card_id = (Str.first_chars card_id 7) in
    let state = Random.State.make_self_init () in
    let id = 
      Uuidm.(v4_gen state () |> to_string |> short_id)
    in
    match id_exists id with
    | true  -> generate id_exists
    | false -> id
end

let create (id: Id.t) content =
  if content = "" then
    Error "content cannot be empty"
  else
    Ok {id; content; last_reviewed_at = Unix.time ()}


let title card = Base.(String.split_lines card.content |> List.hd_exn)



module Rating = struct
  type t = Bad | Again | Good | Easy
  [@@deriving show]

  let from_int = function
    | 0 -> Ok Bad
    | 1 -> Ok Again
    | 2 -> Ok Good 
    | 3 -> Ok Easy
    | _ -> Error "Rating must be in the range [0 - 3]"

  let of_string rating_str = 
    match rating_str with
    | "bad" -> Ok Bad
    | "again" -> Ok Again
    | "good" -> Ok Good 
    | "easy" -> Ok Easy
    | _ -> Error "Rating must be one of bad | again | good | easy"

  let to_string = function
    | Bad -> "Bad"
    | Again -> "Again"
    | Good -> "Good "
    | Easy -> "Easy"
end
