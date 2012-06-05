(******************************************************************************
 *                             Core-extended                                  *
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

module List = Core.Std.List

module type S = sig
  include Core.Monad.S

  (* Like [List.map] but for functions which return monads *)
  val map_monad : 'a list -> f : ('a -> 'b monad) -> 'b list monad

  (* Like [map_monad] but ignores the outputs from the function. *)
  val map_monad_ignore : 'a list -> f : ('a -> unit monad) -> unit monad
end

module Make (M : Core.Monad.Basic) : S with type 'a monad = 'a M.t = struct
  include Core.Monad.Make (M)

  (*
   * Converts a list of monadic values into a monadic list of values
   * val sequence : 'a monad list -> 'a list monad
   *)
  let sequence monad_list =
    List.fold_right monad_list ~init:(return [])
      ~f:(fun m acc_m -> m >>= fun x -> acc_m >>= fun acc -> return (x :: acc))

  (*
   * Converts a list of monadic units into a monadic unit
   * (ignoring the returned unit values of the monad)
   * val sequence_ignore : unit monad list -> unit monad
   *)
  let sequence_ignore monad_list =
    List.fold ~f:(fun m1 m2 -> m1 >>= fun () -> m2) ~init:(return ()) monad_list

  let map_monad list ~f = sequence (List.map ~f list)

  let map_monad_ignore list ~f = sequence_ignore (List.map ~f list)
end

module type S2 = sig
  include Core.Monad.S2

  val map_monad : 'a list -> f : ('a -> ('b, 'c) monad) -> ('b list, 'c) monad

  val map_monad_ignore : 'a list -> f : ('a -> (unit, 'b) monad) -> (unit, 'b) monad
end

module Make2 (M : Core.Monad.Basic2) : S2 with type ('a,'b) monad = ('a,'b) M.t = struct
  include Core.Monad.Make2 (M)

  let sequence monad_list =
    List.fold_right monad_list ~init:(return [])
      ~f:(fun m acc_m -> m >>= fun x -> acc_m >>= fun acc -> return (x :: acc))

  let sequence_ignore monad_list =
    List.fold ~f:(fun m1 m2 -> m1 >>= fun () -> m2) ~init:(return ()) monad_list

  let map_monad list ~f = sequence (List.map ~f list)

  let map_monad_ignore list ~f = sequence_ignore (List.map ~f list)
end
