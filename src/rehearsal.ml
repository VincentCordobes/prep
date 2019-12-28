open Base
open Stdio
open Console

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


module Interval = struct
  open Yojson.Safe.Util

  type t = Day of int | Week of int [@@deriving show]

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

end

module Box = struct
  type t = {
    interval: Interval.t;
    cards: Card.t list;
  } [@@deriving show, yojson]


  let create ?(cards = []) interval  =
    {interval; cards}

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
    Interval.equals a.interval b.interval
end

let exit_card_not_found card_id = 
  Out_channel.fprintf stderr "No card found with id %s\n" card_id;
  Caml.exit 1


module Db = struct
  (** sorted *)
  type t = {boxes: Box.t list} 
  [@@deriving show, yojson {exn = true}]


  let add_box box sp =
    {
      boxes =
        List.sort (box :: sp.boxes) ~compare:(fun a b ->
            Interval.compare a.interval b.interval);
    }

  let add card ?(at = 0) sp =
    let rec add i boxes =
      match boxes with
      | [] -> [Box.add card (Box.create (Interval.Day 1))]
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

  let find_card card_id sp =
    let box_cards =  
      List.foldi
        sp.boxes
        ~init:(Hashtbl.Poly.create ()) 
        ~f:(fun i table box -> 
            List.iter box.cards ~f:(fun card -> 
                Hashtbl.add table ~key:card.id ~data:(i, card) |> ignore
              ) ;
            table) in
    Hashtbl.find box_cards card_id


  let find_card_or_exit card_id sp =
    match find_card card_id sp with
    | Some result -> result
    | None -> exit_card_not_found card_id 


  let exists card_id sp =
    let result = find_card card_id sp in
    match result with
    | Some _ -> true
    | None -> false


  let move_card_to to_box card_id sp =
    let boxes_count = List.length sp.boxes in
    if to_box < 0 || to_box >= boxes_count then
      sp
    else
      let from_box, card = find_card_or_exit card_id sp in
      let boxes =
        List.mapi sp.boxes ~f:(fun i box ->
            if i = from_box then Box.remove card_id box
            else if i = to_box then Box.add card box
            else box)
      in

      {boxes}

  (* let list_today db = *)
  (*   let box = List.find db.boxes ~f:(fun box -> box.interval *)

end


let editor_template = 
      {|

# Please enter the content here. Lines starting with 
# '#' will be ignored, and so does an empty content.|}

let edit_in_editor text =
  let temp_file, outc = Caml.Filename.open_temp_file "editor" "" in

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
        Caml.Sys.command (Caml.Filename.quote bin ^ " " ^ temp_file ^ " " ^ opts) <> 127)
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
  with Failure msg -> Out_channel.fprintf stderr "%s\n" msg




let add_box interval =
  let db = Db.load () in
  let box_exists =
    List.exists db.boxes ~f:(fun box ->
        Interval.compare box.interval interval = 0)
  in
  if box_exists then
    print_error "A box with interval %a already exists" green_s
    @@ Interval.to_string interval
  else Db.save (Db.add_box {interval; cards = []} db)



let list_boxes () =
  let db = Db.load () in
  List.iter db.boxes ~f:(fun {interval; cards} ->
      printf "Every %s\n" @@ Interval.to_string interval;
      if List.length cards > 0 then (
        List.iter cards ~f:(fun card ->
            printf "* %s %s\n" card.id @@ Card.title card);
        printf "\n" ))

let show_card id =
  let db = Db.load () in
  let _, card = Db.find_card_or_exit id db in
  printf "%s\n" card.content


let edit card_id =
  let db = Db.load () in
  let box_id, card = Db.find_card_or_exit card_id db in
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


let remove card_id =
  let db = Db.load () in
  let box_id, card = Db.find_card_or_exit card_id db in
  let sp : Db.t =
    {
      boxes =
        List.mapi db.boxes ~f:(fun i box ->
            if i = box_id then Box.remove card.id box else box);
    }
  in
  Db.save sp;
  printf "card removed\n"


let move_card card_id box_id =
  let db = Db.load () in
  Db.move_card_to (box_id - 1) card_id db
  |> Db.save


let move_up card_id =
  let db = Db.load () in
  let box_id, _ = Db.find_card_or_exit card_id db in
  Db.move_card_to (box_id + 1) card_id db
  |> Db.save


let move_down card_id =
  let db = Db.load () in
  let box_id, _ = Db.find_card_or_exit card_id db in
  Db.move_card_to (box_id - 1) card_id db
  |> Db.save
