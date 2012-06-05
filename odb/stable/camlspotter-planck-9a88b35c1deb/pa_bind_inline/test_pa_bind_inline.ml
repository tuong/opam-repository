let x = 3 + 0
let x = t <|> f
let x = t >>= f
let x = bind t f

let y = t >>= fun x -> f x >>= fun z -> return z
