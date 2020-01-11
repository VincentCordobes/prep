(** sorted *)
open Base

type t = {boxes: Box.t list} 
[@@deriving show, yojson {exn = true}]


let store_path = "store.json"

let load () = 
  let json_value = Yojson.Safe.from_file store_path in
  let boxes = of_yojson_exn json_value in
  boxes

let save store =
  let boxes_json = to_yojson store in
  Yojson.Safe.to_file store_path boxes_json

let add_box box store =
  {
    boxes =
      List.sort (box :: store.boxes) ~compare:(fun a b ->
          Interval.compare a.interval b.interval);
  }

let add card ?(at = 0) store =
  let rec add i boxes =
    match boxes with
    | [] -> [Box.add card (Box.create (Interval.Day 1))]
    | boxe :: boxes ->
      if i = at then Box.add card boxe :: boxes
      else boxe :: add (i - 1) boxes
  in
  let boxes = add 0 store.boxes in
  {boxes}

let all_cards store =
  List.bind store.boxes ~f:(fun box -> box.cards)

let find_card card_id store =
  let box_cards =  
    List.foldi
      store.boxes
      ~init:(Hashtbl.Poly.create ()) 
      ~f:(fun i table box -> 
          List.iter box.cards ~f:(fun card -> 
              Hashtbl.add table ~key:card.id ~data:(i, card) |> ignore
            ) ;
          table) in
  Hashtbl.find box_cards card_id


let find_card_or_exit card_id store =
  match find_card card_id store with
  | Some result -> result
  | None ->
      Console.(print_error "No card found with id %a\n" green_s card_id);
      Caml.exit 1


let exists card_id store =
  let result = find_card card_id store in
  match result with
  | Some _ -> true
  | None -> false


let move_card_to to_box card_id store =
  let boxes_count = List.length store.boxes in
  if to_box < 0 || to_box >= boxes_count then store
  else
    let from_box, card = find_card_or_exit card_id store in
    if from_box = to_box then store
    else
      let boxes =
        List.mapi store.boxes ~f:(fun i box ->
            if i = from_box then Box.remove card_id box
            else if i = to_box then Box.add card box
            else box)
      in
      {boxes}
