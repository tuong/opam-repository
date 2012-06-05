open Planck

module Stream = struct
  module Str = Stream.Make(struct
    type elem = char
    let show_elem = Printf.sprintf "%C"
    let equal_elem (x : char) y = x = y
    module Pos = Position.File
    type attr = Sbuffer.buf
    let default_attr = Sbuffer.default_buf
    let position_of_attr = Sbuffer.position_of_buf
  end)
    
  include Str
  include Sbuffer.Extend(struct
    include Str
    let create_attr buf = buf
    let buf st = attr st
  end)
end

module Parser = struct
  module Base = Pbase.Make(Stream)

  include Base
  include Pbuffer.Extend(Stream)(Base)
end


