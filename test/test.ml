open Prep
open ISO8601.Permissive

let drop_store () =
  let store_path = try Sys.getenv "STORE_PATH" with Not_found -> "" in
  if store_path <> "" && Sys.file_exists store_path then (
    Fmt.pr "Removing existing store";
    Sys.remove store_path )

let before_all () =
  drop_store ();
  Store.init ()

let () = before_all ()

let now = 1582818998.889 (* 2020-02-27T15:56:38Z *)

let%expect_test "List empty default boxes" =
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card.
  |}]

let%expect_test "Add a file card" =
  (* when *)
  Cli.add_file "./knocking on heaven door";
  (* then *)
  [%expect {|Card added (id: knocking_on_heaven_door)|}]

let%expect_test "Add a file card with alias" =
  (* when *)
  Cli.add_file ~name:(Some "greenday") "./toto";
  (* then *)
  [%expect {|Card added (id: greenday)|}]

let%expect_test "Add a card" =
  drop_store ();
  [%expect.output] |> ignore;
  Store.init ();

  (* when *)
  Cli.add @@ Some {|Blink182 - All the small things

  body|};
  (* then *)
  [%expect {|Card added (id: blink182_-_all_the_small_things)|}];
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    2020-07-15 Blink182 - All the small things

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card. |}]

let%expect_test "Card Rating" =
  (* when *)
  Cli.rate ~at:now Card.Rating.Bad "blink182_-_all_the_small_things";
  (* then *)
  [%expect {| Card rated bad |}];
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    2020-03-01 Blink182 - All the small things

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card. |}];

  (* when *)
  Cli.rate
    ~at:(datetime "2020-01-01T11:00:00")
    Card.Rating.Again "blink182_-_all_the_small_things";
  [%expect {| Card rated again |}];
  Cli.list_boxes ();
  (* then *)
  (* should not move the card but still update the last reviewed *)
  [%expect
    {|
    #0 Every 3 days
    2020-01-04 Blink182 - All the small things

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card. |}];

  (* when *)
  Cli.rate ~at:now Card.Rating.Good "blink182";
  [%expect {| Card rated good |}];
  Cli.list_boxes ();
  (* then *)
  (* Should move the card a to the next box*)
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    2020-03-05 Blink182 - All the small things

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card. |}];

  (* when *)
  Cli.rate ~at:now Card.Rating.Again "blink182_-_all_the_small_things";
  [%expect {| Card rated again |}];
  Cli.list_boxes ();
  (* then *)
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    2020-03-05 Blink182 - All the small things

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card.
  |}];

  (* when *)
  Cli.rate ~at:now Card.Rating.Easy "blink182_-_all_the_small_things";
  [%expect {| Card rated easy |}];
  Cli.list_boxes ();
  (* then *)
  (* should move the card at the end *)
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    2020-04-09 Blink182 - All the small things
  |}];

  (* when *)
  Cli.rate ~at:now Card.Rating.Easy "blink182_-_all_the_small_things";
  [%expect {| Card rated easy |}];
  Cli.rate ~at:now Card.Rating.Good "blink182_-_all_the_small_things";
  [%expect {| Card rated good |}];
  Cli.list_boxes ();
  (* then *)
  (* Should not move the card *)
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    2020-04-09 Blink182 - All the small things
  |}]

let%expect_test "Show a card" =
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

  (* partial match in the middle *)
  Cli.show_card "all";
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
  [%expect
    {| 
    Error: Several cards matches id bli.

    The most similar cards are
      * blink182_-_all_the_small_things
      * blink
  |}];
  Cli.remove (fun _ -> Some 'y') "blink";
  [%expect
    {|
    You are about to remove the card blink, continue? [y/N]: Card removed.
  |}]

let%expect_test "Add a box" =
  Cli.add_box (Interval.Day 400) |> ignore;
  [%expect {| Box added (repetitions every 400 days) |}]

let%expect_test "Handle duplicate boxes" =
  Cli.add_box (Interval.Day 400) |> ignore;
  [%expect {| Error: A box with interval 400 days already exists |}];
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    2020-04-09 Blink182 - All the small things

    #4 Every 400 days
    No card.
  |}]

let%expect_test "Edit card content" =
  (* given *)
  let open_in_editor _ = {|yoo

  new body|} in
  (* when *)
  Cli.edit open_in_editor "blink";
  (* then *)
  [%expect {| Edited card blink182_-_all_the_small_things (new name yoo) |}];
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    2020-04-09 yoo

    #4 Every 400 days
    No card.
  |}];
  Cli.show_card "yoo";
  [%expect {|
    yoo

      new body
  |}]

let%expect_test "Remove a card - abort" =
  (* when *)
  Cli.remove (fun _ -> Some 'n') "yoo";
  (* then *)
  [%expect
    {|
    You are about to remove the card yoo, continue? [y/N]: Aborted!
  |}];
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    2020-04-09 yoo

    #4 Every 400 days
    No card.
  |}]

let%expect_test "Remove a card" =
  (* when *)
  Cli.remove (fun _ -> Some 'y') "yo";
  (* then *)
  [%expect
    {|
    You are about to remove the card yoo, continue? [y/N]: Card removed.
  |}];
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card.

    #4 Every 400 days
    No card.
  |}]

let%expect_test "next review date" =
  let add_card content =
    Cli.add ~last_reviewed_at:now @@ Some content;
    [%expect {| Card added (.*) (regexp)|}]
  in

  let rate_card_good card_id =
    Cli.rate ~at:now Card.Rating.Good card_id;
    [%expect {| Card rated good |}]
  in

  add_card "song";
  add_card "sing";
  rate_card_good "sing";

  Cli.list_boxes ();
  (* Note that there are 29 days in in feb 2020 *)
  [%expect
    {|
    #0 Every 3 days
    2020-03-01 song

    #1 Every 1 week
    2020-03-05 sing

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card.

    #4 Every 400 days
    No card.
  |}];

  rate_card_good "sing";
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    2020-03-01 song

    #1 Every 1 week
    No card.

    #2 Every 8 days
    2020-03-06 sing

    #3 Every 6 weeks
    No card.

    #4 Every 400 days
    No card.
  |}];

  rate_card_good "sing";
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    2020-03-01 song

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    2020-04-09 sing

    #4 Every 400 days
    No card.
  |}]

let%expect_test "prep review" =
  (* setup *)
  drop_store ();
  [%expect.output] |> ignore;

  (* given *)
  let store =
    Store.empty_store ()
    |> Store.add_box @@ Box.create @@ Day 3
    |> Store.add
         {
           id = "first";
           content = Plain "first card";
           box = 0;
           deck = Deck.default_id;
           last_reviewed_at = datetime "2020-04-05T11:00:00";
           archived = false;
         }
  in
  Store.init ~store ();
  Cli.review (date "2020-04-04");
  [%expect {|
    2020-04-04  --
    2020-04-08  #1 first card
  |}];
  Cli.review (date "2020-04-05");
  [%expect {|
    2020-04-05  --
    2020-04-08  #1 first card
  |}];
  Cli.review (date "2020-04-06");
  [%expect {|
    2020-04-06  --
    2020-04-08  #1 first card
  |}];
  Cli.review (date "2020-04-07");
  [%expect {|
    2020-04-07  --
    2020-04-08  #1 first card
  |}];
  Cli.review (datetime "2020-04-08T00:00");
  [%expect {|
    2020-04-08  #1 first card
  |}];
  Cli.review (datetime "2020-04-08T10:00");
  [%expect {|
    2020-04-08  #1 first card
  |}];
  Cli.review (datetime "2020-04-08T12:00");
  [%expect {|
    2020-04-08  #1 first card
  |}];
  Cli.review (date "2020-04-09");
  [%expect {|
    2020-04-08  #1 first card
    2020-04-09  --
  |}]

let%expect_test "Box are sorted by interval" =
  drop_store ();
  [%expect.output] |> ignore;
  (* given *)
  let store =
    Store.empty_store ()
    |> Store.add_box (Box.create @@ Day 4)
    |> Store.add_box (Box.create @@ Day 2)
    |> Store.add_box (Box.create @@ Week 2)
    |> Store.add_box (Box.create @@ Day 3)
    |> Store.add_box (Box.create @@ Day 8)
    |> Store.add_box (Box.create @@ Week 1)
  in
  Store.init ~store ();

  (* when *)
  Cli.list_boxes ();

  (* then *)
  [%expect
    {|
    #0 Every 2 days
    No card.

    #1 Every 3 days
    No card.

    #2 Every 4 days
    No card.

    #3 Every 1 week
    No card.

    #4 Every 8 days
    No card.

    #5 Every 2 weeks
    No card.
  |}]

let%expect_test "Decks" =
  drop_store ();
  [%expect.output] |> ignore;

  (* when no deck*)
  Store.init ();
  Cli.list_decks ();
  (* then display default deck*)
  [%expect {|
    * default
  |}];

  (* when adding a new deck*)
  Cli.add_deck "custom_deck";
  Cli.list_decks ();
  (* then *)
  [%expect {|
    * default
      custom_deck
  |}];

  (* when adding a card *)
  Cli.add_file "./dilaudid";
  [%expect.output] |> ignore;
  (* then deck are unchanged *)
  Cli.list_decks ();
  [%expect {|
    * default
      custom_deck |}];
  Cli.list_boxes ();
  (* and it's added to the default deck *)
  [%expect
    {|
    #0 Every 3 days
    2020-07-15 dilaudid

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card. |}];

  (* when reviewing a deck *)
  Cli.review (date "2022-04-05");
  (* then it should only display current deck cards *)
  [%expect {|
    2020-07-15  #1 dilaudid
    2022-04-05  -- |}];

  (* when switching the current deck*)
  Cli.use_deck ~input_char:(fun _ -> None) "custom_deck";
  Cli.list_boxes ();
  (* then *)
  [%expect
    {| 
    Using deck custom_deck 
    #0 Every 3 days
    No card.

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card. |}];

  (* when reviewing a deck *)
  Cli.review (date "2022-04-05");
  (* then it should only review the card of the current deck *)
  [%expect {| No card. |}];

  (* when adding a card to the current deck *)
  Cli.add_file "./vince";
  [%expect.output] |> ignore;
  (* then it shows that card in boxes *)
  Cli.list_boxes ();
  [%expect
    {|
    #0 Every 3 days
    2020-07-15 vince

    #1 Every 1 week
    No card.

    #2 Every 8 days
    No card.

    #3 Every 6 weeks
    No card. |}];
  Cli.review (date "2022-04-05");
  (* and it reviews only that card *)
  [%expect {|
    2020-07-15  #1 vince
    2022-04-05  -- |}]

let%expect_test "use-deck" =
  drop_store ();
  [%expect.output] |> ignore;

  (* when no deck*)
  Store.init ();
  Cli.list_decks ();
  (* then display default deck*)
  [%expect {|
    * default
  |}];

  (* when the deck doesnt exists and we dont want to create it *)
  Cli.use_deck ~input_char:(fun _ -> Some 'n') "toto";
  [%expect
    {|Deck toto doesn't exist. Do you want to create it? [y/N] Aborted!|}];
  Cli.list_decks ();
  (* then it doesn't create it *)
  [%expect {| * default |}];

  (* when the deck doesnt exists and we want to create it *)
  Cli.use_deck ~input_char:(fun _ -> Some 'y') "toto";
  [%expect
    {| 
    Deck toto doesn't exist. Do you want to create it? [y/N] Deck created.
    Using deck toto |}];
  Cli.list_decks ();
  (* then it creates a new one *)
  [%expect {|
      default
    * toto
  |}];

  (* when adding a deck *)
  Cli.add_deck "tata";
  Cli.add_deck "titi";
  Cli.list_decks ();
  (* then *)
  [%expect {|
      default
    * toto
      tata
      titi
  |}]
