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
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    No card.
  |}]

let%expect_test "Add a file card" =
  (* when *)
  Cli.add_file "./knocking on heaven door";
  (* then *)
  [%expect {|Card added (id: knocking_on_heaven_door)|}]

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
    Every 3 days
    \* Blink182 - All the small things (.*) (regexp)
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    No card. |}]

let%expect_test "Card Rating" =
  (* when *)
  Cli.rate ~at:now Card.Rating.Bad "blink182_-_all_the_small_things";
  (* then *)
  [%expect {| Card rated bad |}];
  Cli.list_boxes ();
  [%expect
    {|
    Every 3 days
    \* Blink182 - All the small things (.*) (regexp)
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
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
    Every 3 days
    * Blink182 - All the small things (last 2020-01-01, next 2020-01-04)
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    No card. |}];

  (* when *)
  Cli.rate ~at:now Card.Rating.Good "blink182";
  [%expect {| Card rated good |}];
  Cli.list_boxes ();
  (* then *)
  (* Should move the card a to the next box*)
  [%expect
    {|
    Every 3 days
    No card.
    Every 1 week
    \* Blink182 - All the small things (.*) (regexp)
    Every 8 days
    No card.
    Every 6 weeks
    No card. |}];

  (* when *)
  Cli.rate ~at:now Card.Rating.Again "blink182_-_all_the_small_things";
  [%expect {| Card rated again |}];
  Cli.list_boxes ();
  (* then *)
  [%expect
    {|
    Every 3 days
    No card.
    Every 1 week
    \* Blink182 - All the small things (.*) (regexp)
    Every 8 days
    No card.
    Every 6 weeks
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
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* Blink182 - All the small things (.*) (regexp)
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
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* Blink182 - All the small things (.*) (regexp)
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
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* Blink182 - All the small things (.*) (regexp)
    Every 400 days
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
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* yoo (.*) (regexp)
    Every 400 days
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
    Every 3 days
    No card.
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    \* yoo (.*) (regexp)
    Every 400 days
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
  (* Note that there are 29 days in in feb 2020  *)
  [%expect
    {|
    Every 3 days
    * song (last 2020-02-27, next 2020-03-01) 
    Every 1 week
    * sing (last 2020-02-27, next 2020-03-05)
    Every 8 days
    No card.
    Every 6 weeks
    No card.
    Every 400 days
    No card.
  |}];

  rate_card_good "sing";
  Cli.list_boxes ();
  [%expect
    {|
    Every 3 days
    * song (last 2020-02-27, next 2020-03-01)
    Every 1 week
    No card.
    Every 8 days
    * sing (last 2020-02-27, next 2020-03-06)
    Every 6 weeks
    No card.
    Every 400 days
    No card.
  |}];

  rate_card_good "sing";
  Cli.list_boxes ();
  [%expect
    {|
    Every 3 days
    * song (last 2020-02-27, next 2020-03-01)
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    * sing (last 2020-02-27, next 2020-04-09)
    Every 400 days
    No card.
  |}]

let%expect_test "prep review" =
  (* setup *)
  drop_store ();
  [%expect.output] |> ignore;

  (* given *)
  let card =
    Card.
      {
        id = "awesome_card";
        content = Plain "Awesome card";
        box = 0;
        deck = Deck.default_id;
        last_reviewed_at = datetime "2020-04-05T11:00:00";
      }
  in
  let store =
    Store.empty_store ()
    |> Store.add_box @@ Box.create @@ Day 3
    |> Store.add card
  in
  Store.init ~store ();
  (* when *)
  Cli.review (date "2020-04-05");
  Cli.review (date "2020-04-06");
  Cli.review (date "2020-04-07");
  Cli.review (datetime "2020-04-08T00:00");
  Cli.review (datetime "2020-04-08T10:00");
  Cli.review (datetime "2020-04-08T12:00");
  Cli.review (date "2020-04-09");
  (* then *)
  [%expect
    {| 
    No card. 
    No card. 
    No card. 
    * Awesome card (box #1)
    * Awesome card (box #1)
    * Awesome card (box #1)
    * Awesome card (box #1)
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
    Every 2 days
    No card.
    Every 3 days
    No card.
    Every 4 days
    No card.
    Every 1 week 
    No card.
    Every 8 days
    No card.
    Every 2 weeks
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
    default
    No card.
  |}];

  (* when adding a new deck*)
  Cli.add_deck "custom_deck" None;
  Cli.list_decks ();
  (* then *)
  [%expect {|
    default
    No card.
    custom_deck
    No card.
  |}];

  (* when adding a card *)
  Cli.add_file "./dilaudid";
  [%expect.output] |> ignore;
  Cli.list_decks ();
  (* then it's added to the default deck *)
  [%expect
    {|
    default
    * dilaudid (box #1)
    custom_deck
    No card.
  |}];

  (* when switching the current deck*)
  Cli.use_deck "custom_deck";
  (* then *)
  [%expect {| Using deck custom_deck |}];

  (* when adding a card to the current deck *)
  Cli.add_file "./vince";
  [%expect.output] |> ignore;
  Cli.list_decks ();
  (* then *)
  [%expect
    {|
     default
     * dilaudid (box #1)
     custom_deck
     * vince (box #1)
   |}];

  (* when listing cards *)
  Cli.list_boxes ();

  (* then it should not mix decks *)
  [%expect
    {|
    Every 3 days
    \* vince (.*) (regexp)
    Every 1 week
    No card.
    Every 8 days
    No card.
    Every 6 weeks
    No card. |}]

let%expect_test "use-deck" =
  drop_store ();
  [%expect.output] |> ignore;

  (* when no deck*)
  Store.init ();
  Cli.list_decks ();
  (* then display default deck*)
  [%expect {|
    default
    No card.
  |}];

  (* when the deck doesnt exists *)
  Cli.use_deck "toto";
  [%expect {| 
    Deck toto created
    Using deck toto
  |}];
  Cli.list_decks ();
  (* then it create an empty deck*)
  [%expect {|
    default
    No card.
    toto
    No card.
  |}];

  (* when adding a deck *)
  Cli.add_deck "tata" None;
  Cli.add_deck "titi" None;
  Cli.list_decks ();
  (* then *)
  [%expect
    {|
    default
    No card.
    toto
    No card.
    tata
    No card.
    titi
    No card.
  |}]

