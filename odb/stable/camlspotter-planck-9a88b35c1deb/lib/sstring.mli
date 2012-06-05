(** Positionless string stream (not char stream), used for Sbuffer 

    It is to implement Sbuffer, and not for casual use.
*)

include Stream_intf.S
  with type elem = string
  and  type attr = unit
  and  type Pos.t = unit

val from_string : string -> t

val from_chan : in_channel -> t
(** No auto close at EOF *)

