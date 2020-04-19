open Base
open Stdio

let default_template =
  {|

# Please enter the content here. Lines starting with 
# '#' will be ignored, and so does an empty content.|}

let edit text =
  let temp_file, outc = Caml.Filename.open_temp_file "editor" "" in

  Out_channel.fprintf outc "%s" text;
  Out_channel.close outc;

  let candidates =
    match Caml.Sys.getenv_opt "VISUAL" with
    | Some x -> [ x ]
    | None -> (
        match Caml.Sys.getenv_opt "EDITOR" with
        | Some x -> [ x ]
        | None -> (
            match Caml.Sys.getenv_opt "PAGER" with
            | Some x -> [ x ]
            | None -> [] ) )
  in
  let is_vim =
    List.exists candidates ~f:(fun candidate ->
        List.mem [ "vim"; "nvim" ] candidate ~equal:equal_string)
  in
  let candidates = candidates @ [ "xdg-open"; "open" ] in
  let opts = if is_vim then "-c \"set filetype=gitcommit\"" else "" in
  List.exists
    ~f:(fun bin ->
      Caml.Sys.command (Caml.Filename.quote bin ^ " " ^ temp_file ^ " " ^ opts)
      <> 127)
    candidates
  |> ignore;
  let content = In_channel.read_all temp_file in
  let comments_regex = Str.regexp {|^#.*|} in
  let content = Str.global_replace comments_regex "" content |> String.strip in
  Caml.Sys.remove temp_file;
  content
