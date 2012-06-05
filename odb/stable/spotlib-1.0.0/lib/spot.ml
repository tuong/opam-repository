include Base

module Monad_intf = Monad_intf
module Monad = Monad
module Option = Option
module List = struct
  include List
  include Xlist
end
module Format = struct
  include Format
  include Xformat
end
module Hashtbl = struct
  include Hashtbl
  include Xhashtbl
end
module String = struct
  include String
  include Xstring
end
module Weaktbl = Weaktbl
module Phantom = Phantom
