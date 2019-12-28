let red_s ppf = Fmt.(styled `Red string) ppf
let green_s ppf = Fmt.(styled `Green string) ppf

let print_error fmt  = 
  Caml.(Fmt.pr ("%a: " ^^ fmt ^^ "\n") red_s "Error")
