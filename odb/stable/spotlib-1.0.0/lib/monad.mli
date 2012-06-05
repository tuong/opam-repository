open Monad_intf
module Make(M : S) : T with type 'a t = 'a M.t
module Make2(M : S2) : T2 with type ('a, 'z) t = ('a, 'z) M.t
