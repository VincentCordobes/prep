let rec mkdir_p dir file_perm =
  if Sys.file_exists dir then
    ()
  else begin
    mkdir_p (Filename.dirname dir) file_perm;
    Unix.mkdir dir file_perm
  end

