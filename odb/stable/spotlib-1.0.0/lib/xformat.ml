open Format

type t = formatter

let stdout = std_formatter
let stderr = err_formatter

let rec list sep f ppf = function
  | [] -> ()
  | [x] -> f ppf x
  | x::xs ->
      fprintf ppf "%a%t%a"
        f x
        sep
        (list sep f) xs
