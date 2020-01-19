open Alcotest

let build suite =
  (List.map (fun (name, cb) -> test_case name `Quick cb) suite)


let suite = [
  ("should list cards c where last_reviewed_at(c) + interval(b) = now", fun () -> 
      check int "same string" 4 4
  );

  ("should list cards c where last_reviewed_at(c) + interval(b) = now", fun () -> 
      check int "same string" 4 4
  );
]


let () =
  run "Rehearsal" [
    "review", build suite;
  ]



