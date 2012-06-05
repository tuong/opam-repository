(** Result monad: Haskell's Either but with ``Right'' names. *)

open Spotlib.Spot

type ('a, 'error) _t = 
  | Ok of 'a
  | Error of 'error
with sexp

include Monad_intf.T2 with type ('a, 'error) t = ('a, 'error) _t

val fail : 'error -> ('a, 'error) t
