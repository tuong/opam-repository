include Monad.Make(struct
  type 'a t = 'a option

  let return v = Some v

  let bind t f = match t with
    | Some v -> f v
    | None -> None
end)
