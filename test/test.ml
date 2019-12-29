open Alcotest

let list_cards () =
  check int  "same string" 4 4

let () =
  run "Rehearsal" [
    "list-cards", [
      test_case "date is exactly last(c) + interval(c)" `Quick list_cards;
    ];
  ]
