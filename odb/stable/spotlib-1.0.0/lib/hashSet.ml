type 'a t = ('a, unit) Hashtbl.t (* poorman's implementation *)
let create = Hashtbl.create
let mem = Hashtbl.mem
let add t k = if not (mem t k) then Hashtbl.add t k ()
let remove = Hashtbl.remove

