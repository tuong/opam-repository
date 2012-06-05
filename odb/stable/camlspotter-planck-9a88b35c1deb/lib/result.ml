open Spotlib.Spot

type ('a, 'error) _t =
  | Ok of 'a
  | Error of 'error
with sexp

include Monad.Make2(struct
  type ('a, 'error) t = ('a, 'error) _t
  let return v = Ok v
  let bind v f = match v with
    | Ok v -> f v
    | Error e -> Error e
end)
let fail e = Error e



