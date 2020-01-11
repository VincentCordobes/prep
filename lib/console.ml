let red_s ppf = Fmt.(styled `Red string) ppf
let green_s ppf = Fmt.(styled `Green string) ppf
let magenta_s ppf = Fmt.(styled `Magenta string) ppf

let print_error fmt  = 
  Fmt.(pf stderr) ("%a: " ^^ fmt ^^ "\n") red_s "Error"
