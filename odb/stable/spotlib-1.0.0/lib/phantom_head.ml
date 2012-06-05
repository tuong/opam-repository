type ('phantom, 'content) t = 'content

module Open = struct
  type unknown
  let unknown : unknown = Obj.magic 0
  let (!<) x = x
  let (!>) x = x
  let (!?) x = x
end
include Open
let unsafe x = x
let magic x = x

let map f x = f x
let combine x y = (x,y)

type ('phantom, 'content) ts = 'content list

