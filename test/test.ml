open Prep


let drop_store () =
  let store_path = 
    try Sys.getenv "STORE_PATH" with
    | Not_found -> "" in
  if store_path <> "" && Sys.file_exists store_path then
    begin
      Fmt.pr "Removing existing store";
      Sys.remove store_path
    end


let before_all () = drop_store()

let () = before_all ()


let%expect_test "List empty default boxes" =
  Store.init ();
  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    No card.
  |}]


let%expect_test "Add and rate a card" =
  Cli.add @@ Some {|Blink182 - All the small things

body|};
  [%expect {|Card added (id: blink182_-_all_the_small_things)|}];

  let expect_some_boxes_with_one_card () =
    Cli.list_boxes ();
    [%expect{|
      Every 3 days
      \* Blink182 - All the small things (last reviewed on .*) (regexp)
      Every 1 week
      No card.
      Every 8 days
      No card.
      Every 6 weeks
      No card. |}] in
  expect_some_boxes_with_one_card ();

  (* Card Rating *)
  (* Should not move the card *)
  Cli.rate Card.Rating.Bad "blink182_-_all_the_small_things";
  [%expect {| Card rated bad |}];
  expect_some_boxes_with_one_card ();

  Cli.rate Card.Rating.Again "blink182_-_all_the_small_things";
  [%expect {| Card rated again |}];
  expect_some_boxes_with_one_card ();

  (* Should move the card a to the next box*)
  Cli.rate Card.Rating.Good "blink182";
  [%expect {| Card rated good |}];
  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    \* Blink182 - All the small things (last reviewed on .*) (regexp)
    Every 8 days
    No card.
    Every 6 weeks
    No card.
  |}];

  (* Should move the card at this end *)
  Cli.rate Card.Rating.Easy "blink182_-_all_the_small_things";
  [%expect {| Card rated easy |}];
  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* Blink182 - All the small things (last reviewed on .*) (regexp)
  |}];

  (* Should not move the card *)
  Cli.rate Card.Rating.Easy "blink182_-_all_the_small_things";
  [%expect {| Card rated easy |}];
  Cli.rate Card.Rating.Good "blink182_-_all_the_small_things";
  [%expect {| Card rated good |}];
  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* Blink182 - All the small things (last reviewed on .*) (regexp)
  |}];


  (* Show a card *)
  (try Cli.show_card "azerty" with _ -> ());
  [%expect {|
    Error: No card found with id azerty
  |}];

  Cli.show_card "blink182_-_all_the_small_things";
  [%expect {|
    Blink182 - All the small things 

    body
  |}];

  (* Partial match *)
  Cli.show_card "bliNk182";
  [%expect {|
    Blink182 - All the small things 

    body
  |}];

  (* Partial match *)
  Cli.show_card "blink182_-_all_the_small_thingss";
  [%expect {|
    Blink182 - All the small things 

    body
  |}];

  (* Exact match *)
  Cli.add @@ Some {|blink|};
  [%expect {| Card added (id: blink) |}];

  Cli.show_card "blink";
  [%expect {| blink |}];

  (* Ambigous match *)
  (try Cli.show_card "bli" with _ -> ());
  [%expect {| 
    Error: Several cards matches id bli.

    The most similar cards are
      * blink182_-_all_the_small_things
      * blink
  |}];
  Cli.remove (fun _ -> Some 'y') "blink";
  [%expect{|
    You are about to remove the card blink, continue? [y/N]: Card removed.
  |}];


  (* Add a box *)
  Cli.add_box (Interval.Day 400) |> ignore;
  [%expect {| Box added (repetitions every 400 days) |}];

  (* Handle duplicate boxes *)
  Cli.add_box (Interval.Day 400) |> ignore;
  [%expect {| Error: A box with interval 400 days already exists |}];

  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* Blink182 - All the small things (last reviewed on .*) (regexp)
    Every 400 days
    No card.
  |}];


  (* Edit card content *)
  let open_in_editor _  = 
    {|yo

new body|} 
  in
  Cli.edit open_in_editor "blink";
  [%expect {| Edited card blink182_-_all_the_small_things (new name yo) |}];

  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* yo (last reviewed on .*) (regexp)
    Every 400 days
    No card.
  |}];

  Cli.show_card "yo";
  [%expect {|
    yo 

    new body
  |}];



  (* Remove a card confirmation *)
  Cli.remove (fun _ -> Some 'n') "yo";
  [%expect{|
    You are about to remove the card yo, continue? [y/N]: Aborted!
  |}];

  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* yo (last reviewed on .*) (regexp)
    Every 400 days
    No card.
  |}];

  (* Remove a card *)
  Cli.remove (fun _ -> Some 'y') "yo";
  [%expect{|
    You are about to remove the card yo, continue? [y/N]: Card removed.
  |}];

  Cli.list_boxes ();
  [%expect{|
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    No card.
    Every 400 days
    No card.
  |}];

