open Plang

let blank = ignore (one_of [' '; '\n'; '\r'; '\t'])
 
let rec expr () = 
  (blank >>= fun () -> expr ())

  <|> (token '+' >>= fun () -> return 0)


  
