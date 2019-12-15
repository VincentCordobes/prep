open Alcotest

module To_test = struct
  let lowercase = fun x -> x 
  let capitalize = String.capitalize_ascii
  let str_concat = String.concat ""
  let list_concat = List.append
end

(* The tests *)
let test_lowercase () =
  check int "same string" 4 (To_test.lowercase 4)

let test_capitalize () =
  check string "same string" "World." (To_test.capitalize "world.")


(* Run it *)
let () =
  run "Utils" [
    "string-case", [
      test_case "Lower case"     `Quick test_lowercase;
      test_case "Capitalization" `Quick test_capitalize;
    ];
  ]
