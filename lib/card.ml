type t = {
  id: string; [@printer fun fmt -> fprintf fmt "%s"]
  content: string;
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
    Ok {id; content}


let title card = Base.(String.split_lines card.content |> List.hd_exn)

