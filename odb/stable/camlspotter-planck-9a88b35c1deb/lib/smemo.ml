open Result

type memo_result = (Obj.t, exn) Result.t

module Extend(Base : sig
  include Stream_intf.S
  module Memo : Hashtbl.S
  val memo : t -> memo_result Memo.t
end) = struct
  open Base

  type t = (Obj.t, exn) Result.t Memo.t

  (* CR jfuruse: key equality is by polymoprhic equal!!! 
     Possible bottle neck!
  *)
  (* key uniqueness is the user's responsibility *)
  let memoize (key : Memo.key) (f : (Base.t -> 'a)) (str : Base.t) : 'a =
    let m = memo str in
    let res = 
      try Memo.find m key with
      | Not_found ->
          let res = 
            try Ok (Obj.repr (f str)) with
            | exn -> Error exn
          in
          Memo.replace m key res;
          res
    in
    match res with
    | Ok res -> Obj.obj res (* Type safe! Only if the key is unique... *)
    | Error exn -> raise exn

end
