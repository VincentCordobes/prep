type content =
  | File of string option * string
  | Plain of string

let pp_content fmt content =
  match content with
  | Plain text -> Fmt.pf fmt "%s" text
  | File (name, path) -> (
      match name with
      | Some name -> Fmt.pf fmt "%s (%s)" name path
      | None -> Fmt.pf fmt "%s" path )

let content_to_yojson content =
  match content with
  | Plain text -> `Assoc [ ("type", `String "Plain"); ("value", `String text) ]
  | File (name, path) -> (
      match name with
      | Some name ->
          `Assoc
            [
              ("type", `String "File");
              ("path", `String path);
              ("name", `String name);
            ]
      | None -> `Assoc [ ("type", `String "File"); ("path", `String path) ] )

let content_of_yojson content =
  let open Yojson.Safe.Util in
  let content_type = content |> member "type" |> to_string in
  match content_type with
  | "Plain" ->
      let value = member "value" content |> to_string in
      Ok (Plain value)
  | "File" ->
      let path = member "path" content |> to_string in
      let name = member "name" content |> to_string_option in
      Ok (File (name, path))
  | _ -> Error "Invalid card content"

type t = {
  id : string;
  content : content; [@printer pp_content]
  box : int;
  deck : string; [@default Deck.default_id]
  last_reviewed_at : float;
  archived : bool; [@default false]
}
[@@deriving show, yojson]

module Id = struct
  type t = string

  let rec generate id_exists =
    let short_id card_id = Str.first_chars card_id 7 in
    let state = Random.State.make_self_init () in
    let id = Uuidm.(v4_gen state () |> to_string |> short_id) in
    match id_exists id with true -> generate id_exists | false -> id
end

let create id ?(deck = Deck.default_id) content last_reviewed_at =
  match content with
  | Plain text | File (_, text) ->
      if text = "" then
        Error "content cannot be empty"
      else
        Ok { id; content; box = 0; last_reviewed_at; deck; archived = false }

let generate_id content =
  let open Base in
  String.split_lines content |> List.hd_exn |> String.lowercase
  |> String.substr_replace_all ~pattern:" " ~with_:"_"

let title card =
  match card.content with
  | Plain text -> Base.(String.split_lines text |> List.hd_exn)
  | File (name, path) -> (
      match name with Some name -> name | None -> Filename.basename path )

module Rating = struct
  type t =
    | Bad
    | Again
    | Good
    | Easy
  [@@deriving show]

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
