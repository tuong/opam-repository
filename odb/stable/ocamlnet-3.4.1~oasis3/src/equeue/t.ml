let fd_cmp =
  if Sys.os_type = "Unix" then
    (fun (fd1:Unix.file_descr) fd2 ->
       Pervasives.compare (Obj.magic fd1 : int) (Obj.magic fd2 : int)
    )
  else
    Pervasives.compare


