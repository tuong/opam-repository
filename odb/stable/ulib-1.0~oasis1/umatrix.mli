(***********************************************************************)
(*                                                                     *)
(*                              My_stdlib                              *)
(*                                                                     *)
(*             projets Estime & Cristal, INRIA-Rocquencourt            *)
(*                                                                     *)
(*  Copyright 2004-2007 INRIA.                                         *)
(*  Institut National de Recherche en Informatique et en Automatique.  *)
(*  All rights reserved. This file is distributed only by permission.  *)
(*                                                                     *)
(*  Francois Clement <Francois.Clement@inria.fr>                       *)
(*  Pierre Weis <Pierre.Weis@inria.fr>                                 *)
(*                                                                     *)
(***********************************************************************)

(* $Id: umatrix.mli,v 1.4 2010-07-05 08:31:09 weis Exp $ *)

(** Matrix operations.

   A matrix is a two-dimensional array, or equivalently an array of values
   such that each value is an array of a fixed length.
   An element in a matrix is indexed by a pair of integers and accessed in
   first-index-major order, that is, "à la C" or using the lexicographic
   ordering for pairs of integers.

   Following the classical mathematical convention, the first index to
   access an element refers to the row number and is denoted by [i],
   and conversely the second index refers to the column number and is
   denoted by [j]. As usual, the row numbers are increasing from top
   to bottom and the column numbers are increasing from left to right.

   Beware: these notions are conventional and subject to modification
   according to the problem at hand. For instance, in graphical
   applications the row numbers may decrease from top to bottom. *)

type 'a t = 'a array array;;
(** The type of matrices of element of type ['a]. *)

type 'a matrix = 'a t;;
(** ['a matrix] is an alias for the type ['a] {!t} to be used
   in the module {!Block}. *)

val size : 'a t -> int * int;;
(** Return the size of the given matrix.
    The size of a matrix is a pair of integers that gives the number
    of elements in both directions.

    Raise [Invalid_argument] if the given array is not a valid matrix,
    that is, if it is not an array of arrays that all have the same
    length. *)

val length : 'a t -> int * int;;
(** [length] is an alias for {!size}. *)

val dim_i : 'a t -> int;;
(** [dim_i m] returns the first dimension of the matrix [m].
   Hence, the first index of [m] varies from 0 to [dim_i m - 1]. *)

val dim_j : 'a t -> int;;
(** [dim_j m] returns the second dimension of the matrix [m].
   Hence, the second index of [m] varies from 0 to [dim_j m - 1]. *)

val dim1 : 'a t -> int;;
(** [dim1] is an alias for {!dim_i}. *)

val dim2 : 'a t -> int;;
(** [dim2] is an alias for {!dim_j}. *)

val get : 'a t -> int -> int -> 'a;;
(** [get m i j] returns the element number [(i, j)] of matrix [m].
   You can also write [m.(i).(j)] instead of [get m i j].
   
   Raise [Invalid_argument] if [i] is outside the range
   0 to [(]{!dim_i} [m - 1)] or [j] is outside the range
   0 to [(]{!dim_j} [m - 1)]. *)

val set : 'a t -> int -> int -> 'a -> unit;;
(** [set m i j x] modifies matrix [m] in place, replacing
   element number [(i, j)] with [x].
   You can also write [m.(i).(j) <- x] instead of [set m i j x].

   Raise [Invalid_argument], if [i] is outside the range
   0 to [(]{!dim_i} [m - 1)] or [j] is outside the range
   0 to [(]{!dim_j} [m - 1)]. *)

val make : int -> int -> 'a -> 'a t;;
(** [make ni nj x] returns a fresh matrix with size [(ni, nj)].
   All the elements of this new matrix are initially physically equal to [x].

   Raise [Invalid_argument], if [ni] or [nj] is negative or
   greater than {!Sys.max_array_length}.
   If the value of [x] is a floating-point number, then the maximum
   size is only {!Sys.max_array_length} [/ 2]. *)

val init : int -> int -> (int -> int -> 'a) -> 'a t;;
(** [init ni nj f] returns a fresh matrix of size [(ni, nj)],
   with element number [(i, j)] initialized to the result of [f i j].
   In other terms, it tabulates the results of [f] applied to the pairs of
   integers [(0, 0)] to [(ni - 1, nj - 1)].
   
   Raise [Invalid_argument], if [ni] or [nj] is negative or
   greater than {!Sys.max_array_length}.
   If the return type of [f] is [float], then the maximum
   size is only {!Sys.max_array_length} [/ 2]. *)

val printer : 'a Uprinter.t -> 'a t Uprinter.t;;
(** [printer print ob m] prints the matrix [m] on the output buffer [ob]
    by evaluating [Uarray.line_printer print ob] on all lines of the
    matrix. *)

val compact_printer : 'a Uprinter.t -> 'a t Uprinter.t;;
(** Same as [printer] but with compact output. *)

val append_i : 'a t -> 'a t -> 'a t;;
(** [append_i m1 m2] returns a fresh matrix containing the
   concatenation along the first direction of the matrices [m1] and [m2].

   Raise [Invalid_argument], if the two matrices have different second
   dimensions. *)

val append_j : 'a t -> 'a t -> 'a t;;
(** [append_j m1 m2] returns a fresh matrix containing the
   concatenation along the second direction of the matrices [m1] and [m2].

   Raise [Invalid_argument], if the two matrices have different first
   dimensions. *)

val append1 : 'a t -> 'a t -> 'a t;;
(** [append1] is an alias for {!append_i}. *)

val append2 : 'a t -> 'a t -> 'a t;;
(** [append2] is an alias for {!append_j}. *)

val append : 'a t -> 'a t -> 'a t -> 'a t -> 'a t;;
(** [append m1 m2 m3 m4] returns a fresh matrix containing the
   concatenation of the matrices [m1], [m2]. [m3] and [m4].
   It returns the same result as {!append_i}
   [(]{!append_j} [m1 m2) (]{!append_j} [m3 m4)]. *)

val concat_i : 'a t list -> 'a t;;
(** Same as {!append_i}, but concatenates a list of matrices. *)

val concat_j : 'a t list -> 'a t;;
(** Same as {!append_j}, but concatenates a list of matrices. *)

val concat1 : 'a t list -> 'a t;;
(** [concat1] is an alias for {!concat_i}. *)

val concat2 : 'a t list -> 'a t;;
(** [concat2] is an alias for {!concat_j}. *)

val concat : 'a t list list -> 'a t;;
(** [concat mll] returns a fresh matrix containing the concatenation
   of the matrices of the list of lists [mll].
   It returns the same result as . *)

val sub : 'a t -> int -> int -> int -> int -> 'a t;;
(** [sub m oi li oj lj] returns a fresh matrix of size [(li, lj)],
   containing the elements number [(oi, oj)] to [(oi + li - 1, oj + lj - 1)]
   of matrix [m].

   Raise [Invalid_argument], if [oi], [li], [oj], and [lj] do not
   designate a valid submatrix of [m]; that is,
   if [oi < 0], or [li < 0], or [oi + li > ] {!dim_i} [m],
   or [oj < 0], or [lj < 0], or [oj + lj > ] {!dim_j} [m]. *)

val copy : 'a t -> 'a t;;
(** [copy m] returns a copy of [m], that is, a fresh matrix
   containing the same elements as [m]. *)

val fill : 'a t -> int -> int -> int -> int -> 'a -> unit;;
(** [fill m oi li oj lj x] modifies the matrix [m] in place,
   storing [x] in elements number [(oi, oj)] to [(oi + li - 1, oj + lj - 1)].

   Raise [Invalid_argument], if [oi], [li], [oj] and [lj] do not
   designate a valid submatrix of [m] (see {!sub}). *)

val blit : 'a t -> int -> int -> 'a t -> int -> int -> int -> int -> unit;;
(** [blit m1 oi1 oj1 m2 oi2 oj2 li lj] copies [li * lj] elements
   from matrix [m1], starting at element number [(oi1, oj1)], to matrix [m2],
   starting at element number [(oi2, oj2)].
   It works correctly even if [m1] and [m2] are the same matrix,
   and the source and destination chunks overlap.

   Raise [Invalid_argument], if [oi1], [li], [oj1] and [lj] do not designate
   a valid submatrix of [m1], or if [oi2], [li], [oj2] and [lj] do not
   designate a valid submatrix of [m2]. *)

val transpose : 'a t -> 'a t;;
(** [transpose m] returns the transposed of matrix [m], that is,
   a fresh matrix of size [(]{!dim_j}[ m, ]{!dim_i}[ m)]
   whose element number [(i, j)] is equal to [m.(j).(i)].

   Transposition is an involutive operation, except for matrices with at least
   a zero dimension, in which case it returns the empty matrix [[||]]
   of size (0, 0). *)

val to_array : 'a t -> 'a array;;
(** [to_array m] returns a fresh array containing the concatenation
   of all subarrays along the first direction of matrix [m].
   It is equivalent to {!Array.concat}[ [m.(0); m.(1); ...; m.(ni - 1)]]
   where [ni] is the first dimension of matrix [m]. *)

val of_array : 'a array -> int -> int -> 'a t;;
(** [of_array a ni nj] returns a fresh matrix containing the split of
   the array [a] into [ni] subarrays of length [nj].

   Raise [Invalid_argument], if {!Array.length}[ a <> ni * nj]. *)

val to_arraylist : 'a t -> 'a array list;;
(** [to_arraylist m] returns the list of the arrays along the first
   direction of matrix [m].
   It is equivalent to [[ m.(0); m.(1); ...; m.(ni) ]] where [ni] is the first
   dimension of matrix [m]. *)

val of_arraylist : 'a array list -> 'a t;;
(** [of_arraylist al] returns a fresh matrix containing the arrays
   of the array list [al].

   Raise [Invalid_argument], if all the arrays of the list do not have
   the same length. *)

val to_list : 'a t -> 'a list list;;
(** [to_list m] returns the list of lists of all the elements of [m]. *)

val of_list : 'a list list -> 'a t;;
(** [of_list l] returns a fresh matrix containing the elements
   of the list of lists [l]. *)

(** {6 Iterators} *)

val iter : ('a -> unit) -> 'a t -> unit;;
(** [iter f m] applies function [f] in turn to all the elements of [m].
   It is equivalent to {!Array.iter}[ f m.(0); ]{!Array.iter}[ f m.(1); ...; ]
   {!Array.iter}[ f m.(ni - 1); ()] where [ni] is the first dimension of the
   matrix [m]. *)

val map : ('a -> 'b) -> 'a t -> 'b t;;
(** [map f m] applies function [f] to all the elements of [m],
   and builds a matrix with the results returned by [f]:
   [\[| ]{!Array.map}[ f m.(0); ]{!Array.map}[ f m.(1); ...; ]
   {!Array.map}[ f a.(ni - 1) |\]] where [ni] is the first dimension of the
   matrix [m]. *)

val iterij : (int -> int -> 'a -> unit) -> 'a t -> unit;;
(** Same as {!iter}, but the function is applied to the indices
   of the element as first and second arguments, and the element itself as
   third argument. *)

val mapij : (int -> int -> 'a -> 'b) -> 'a t -> 'b t;;
(** Same as {!map}, but the function is applied to the indices
   of the element as first and second arguments, and the element itself as
   third argument. *)

val iter_i : ('a array -> unit) -> 'a t -> unit;;
(** [iter_i f m] applies function [f] in turn to all the rows of [m].
   It is equivalent to {!Array.iter}[ f m]. *)

val iter_j : ('a array -> unit) -> 'a t -> unit;;
(** Same as {!iter_i}, but applied to the columns of the matrix.
   It is equivalent to {!iter_i}[ f (]{!transpose} [m)]. *)

val map_i : ('a array -> 'b array) -> 'a t -> 'b t;;
(** [map_i f m] applies function [f] in turn to all the rows of [m].
   It is equivalent to {!Array.map}[ f m]. *)

val map_j : ('a array -> 'b array) -> 'a t -> 'b t;;
(** Same as {!map_i}, but applied to the columns of the matrix.
   It is equivalent to {!transpose}[ (]{!map_i}[ f (]
   {!transpose} [m))]. *)

val fold_left : ('a -> 'b -> 'a) -> 'a -> 'b t -> 'a;;
(** [fold_left f x m] computes
   {!Array.fold_left}[ f (... (]{!Array.fold_left}[ f
   (]{!Array.fold_left}[ f x m.(0)) m.(1)) ...) m.(ni - 1)],
   where [ni] is the first dimension of the matrix [m]. *)

val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b;;
(** [fold_right f m x] computes
   {!Array.fold_right}[ f m.(0) (]{!Array.fold_right}[ f m.(1) ( ...
   (]{!Array.fold_right}[ f m.(ni - 1) x) ...))],
   where [ni] is the first dimension of the matrix [m]. *)

val subiter : ('a -> unit) -> 'a t -> int -> int -> int -> int -> unit;;
(** [subiter f m oi li oj lj] applies function [f] in turn to the
   elements number [(oi, oj)] to [(oi + li - 1, oj + lj - 1)] of matrix [m].
   It is equivalent to
   {!Uarray.subiter}[ f m.(oi) oj lj;]
   {!Uarray.subiter}[ f m.(oi + 1) oj lj; ...;]
   {!Uarray.subiter}[ f m.(oi + li - 1) oj lj; ()]. *)

val subiterij :
    (int -> int -> 'a -> unit) -> 'a t -> int -> int -> int -> int -> unit;;
(** Same as {!subiter}, but the function is applied to the indices of
   the element as first and second arguments, and the element itself as third
   argument. *)

(** {6 Iterators on two matrices} *)

val iter2 : ('a -> 'b -> unit) -> 'a t -> 'b t -> unit;;
(** [iter2 f m1 m2] applies function [f] in turn to all the elements
   of [m1] and [m2]. It is equivalent to
   {!Uarray.iter2}[ f m1.(0) m2.(0); ]{!Uarray.iter2}[ f m1.(1) m2.(1);
   ...; ]{!Uarray.iter2}[ f m1.(ni - 1) m2.(ni - 1); ()],
   where [ni] is the first dimension of matrices [m1] and [m2]. *)

val map2 : ('a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t;;
(** [map2 f m1 m2] applies function [f] to all the elements of [m1]
   and [m2], and builds a matrix with the results returned by [f]:
   [\[| ]{!Uarray.map2}[ f m1.(0) m2.(0); ]
   {!Uarray.map2}[ f m1.(1) m2.(1); ...; ]
   {!Uarray.map2}[ f m1.(ni - 1) m2.(ni - 1) |\]],
   where [ni] is the first dimension of matrices [m1] and [m2]. *)

val iterij2 : (int -> int -> 'a -> 'b -> unit) -> 'a t -> 'b t -> unit;;
(** Same as {!iter2}, but the function is applied to the indices of the
   elements as first and second arguments, and the elements themselves as
   third and fourth arguments. *)

val mapij2 : (int -> int -> 'a -> 'b -> 'c) -> 'a t -> 'b t -> 'c t;;
(** Same as {!map2}, but the function is applied to the indices of the
   elements as first and second arguments, and the elements themselves as
   third and fourth arguments. *)

val fold_left2 : ('a -> 'b -> 'c -> 'a) -> 'a -> 'b t -> 'c t -> 'a;;
(** [fold_left2 f x m1 m2] computes
   {!Uarray.fold_left2}[ f (... (]{!Uarray.fold_left2}[ f
   (]{!Uarray.fold_left2}[ f x m1.(0) bm2.(0)) m1.(1) m2.(1)) ...)
   m1.(ni - 1) m2.(ni - 1)],
   where [ni] is the first dimension of the matrices [m1] and [m2]. *)

val fold_right2 : ('a -> 'b -> 'c -> 'c) -> 'a t -> 'b t -> 'c -> 'c;;
(** [fold_right2 f m1 m2 x] computes
   {!Uarray.fold_right2}[ f m1.(0) m2.(0)
   (]{!Uarray.fold_right2}[ f m1.(1) m2.(1) ( ...
   (]{!Uarray.fold_right2}[ f m1.(ni - 1) m2.(ni - 1) x) ...))],
   where [ni] is the first dimension of the matrices [m1] and [m2]. *)

(** {6 Specific applications} *)

val make_diagonal_matrix : 'a -> 'a array -> 'a t;;
(** [make_diagonal_matrix zero v] returns a fresh diagonal matrix with its main
   diagonal filled with elements of the vector [v] and all other entries filled
   with [zero]. *)

val make_int_diagonal_matrix : int array -> int t;;
val make_float_diagonal_matrix : float array -> float t;;
(** Specializations of {!make_diagonal_matrix} for integers and floats with the
   canonical zero value. *)

val resize : int -> int -> 'a t -> 'a t;;
(** [resize ni nj m] returns a fresh matrix of size [(ni, nj)].
   Each element of the given matrix [m] becomes a submatrix of size
 
   Raise [Invalid_arg] if the matrix [m] has a zero dimension and not... . *)

val zoom : int -> int -> 'a t -> 'a t;;
(** [zoom ni nj m] returns a fresh matrix whose dimensions are
   those of [m] multiplied by [ni] and [nj]. Each element of the given matrix
   [m] becomes a submatrix of size [(ni, nj)]. *)

(**/**)
(** {6 Undocumented functions} *)

val unsafe_size : 'a t -> int * int;;
val unsafe_dim_j : 'a t -> int;;
val unsafe_get : 'a t -> int -> int -> 'a;;
val unsafe_set : 'a t -> int -> int -> 'a -> unit;;
