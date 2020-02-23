let red_s ppf = Fmt.(styled `Red string) ppf
let green_s ppf = Fmt.(styled `Green string) ppf
let magenta_s ppf = Fmt.(styled `Magenta string) ppf
let yellow_s ppf = Fmt.(styled `Yellow string) ppf
let cyan_s ppf = Fmt.(styled `Cyan string) ppf
let faint_s ppf = Fmt.(styled `Faint string) ppf

let print_error fmt  = 
  Fmt.(pf stderr) ("%a: " ^^ fmt ^^ "\n") red_s "Error"
