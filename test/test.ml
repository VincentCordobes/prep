open Prep


let%expect_test _ =
  (* Should list empty default boxes *)
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
  |}];

  (* Add a card *)
  Cli.add @@ Some "Blink182 - All the small things";
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
  Cli.rate Card.Rating.Good "blink182_-_all_the_small_things";
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
