(******************************************************************************
 *                             Core                                           *
 *                                                                            *
 * Copyright (C) 2008- Jane Street Holding, LLC                               *
 *    Contact: opensource@janestreet.com                                      *
 *    WWW: http://www.janestreet.com/ocaml                                    *
 *                                                                            *
 *                                                                            *
 * This library is free software; you can redistribute it and/or              *
 * modify it under the terms of the GNU Lesser General Public                 *
 * License as published by the Free Software Foundation; either               *
 * version 2 of the License, or (at your option) any later version.           *
 *                                                                            *
 * This library is distributed in the hope that it will be useful,            *
 * but WITHOUT ANY WARRANTY; without even the implied warranty of             *
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU          *
 * Lesser General Public License for more details.                            *
 *                                                                            *
 * You should have received a copy of the GNU Lesser General Public           *
 * License along with this library; if not, write to the Free Software        *
 * Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA  *
 *                                                                            *
 ******************************************************************************)

open Core_hashtbl_intf

module Hashtbl = Core_hashtbl

module Table_sig (Key : T0) = Monomorphic (Hashtbl) (Key)

module type S = sig
  type hashable
  module Hashable : sig
    type t = hashable
  end
  
  
  val hash : hashable -> int
  module Table : Table_sig (Hashable).S
  module Hash_set : Hash_set_intf.S with type elem = hashable
  module Hash_queue : Hash_queue.S with type Key.t = hashable
  module Hash_heap : Hash_heap.S with type Key.t = hashable
end

module Make (T : Hashtbl.Key) : S with type hashable = T.t = struct
  include T
  type hashable = t
  module Hashable = struct
    type t = hashable
  end
  module Table = Hashtbl.Make (T)
  module Hash_set = Hash_set.Make (T)
  module Hash_queue = Hash_queue.Make (T)
  module Hash_heap = Hash_heap.Make (T)
end

module type S_binable = sig
  type hashable
  module Hashable : sig
    type t = hashable
  end
  val hash : hashable -> int
  module Table : sig
    include Core_hashtbl_intf.Monomorphic (Hashtbl) (Hashable).S
    include Binable.S1 with type 'a binable = 'a t
  end
  module Hash_set : Hash_set_intf.S_binable with type elem = hashable
  module Hash_queue : Hash_queue.S with type Key.t = hashable
  module Hash_heap : Hash_heap.S with type Key.t = hashable
end

module Make_binable (T : sig
  include Hashtbl.Key
  include Binable.S with type binable = t
end) : S_binable with type hashable = T.t = struct
  module Table = Hashtbl.Make_binable (T)
  include T
  type hashable = t
  module Hashable = struct
    type t = hashable
  end
  module Hash_set = Hash_set.Make_binable (T)
  module Hash_queue = Hash_queue.Make (T)
  module Hash_heap = Hash_heap.Make (T)
end
