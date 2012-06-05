(* File: lacaml.mli

   Copyright (C) 2010-

     Christophe Troestler
     email: Christophe.Troestler@umons.ac.be
     WWW: http://math.umons.ac.be/an/

   This library is free software; you can redistribute it and/or
   modify it under the terms of the GNU Lesser General Public
   License as published by the Free Software Foundation; either
   version 2 of the License, or (at your option) any later version.

   This library is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
*)

(** Binding to the {{:http://www.netlib.org/blas/}BLAS} and
    {{:http://www.netlib.org/lapack/}LAPACK} libraries.  You can make
    use of this library by referring to the corresponding module you
    need for your precision and number type:
    {[
    open Lacaml.S
    open Lacaml.D
    open Lacaml.C
    open Lacaml.Z]}

    To use this library, you should be familiar with BLAS and LAPACK.  The
    following {{:http://www.netlib.org/blas/blasqr.ps}quick reference
    guide for the BLAS} and
    {{:http://www.netlib.org/lapack/lapackqref.ps}LAPACK quick
    reference} may be useful to you.  For the precise description of
    the functions, consult the man pages
    {{:http://www.math.utah.edu/software/lapack/}online} or, if you
    {{:http://www.netlib.org/lapack/manpages.tgz}installed} them on
    your machine (if you use Linux, they should be in the packages of
    your distribution), read them with Emacs: [M-x man] (under Unix)
    or [M-x woman] (all systems).
 *)

open Bigarray

(** {2 Pretty printing} *)

(** Pretty-printing of vector and matrices. *)
module Io : sig
  open Format

  (** {6 Generic matrix printing functions} *)

  module Context : sig
    type t

    val create : int -> t

    val ellipsis_default : string ref
    val vertical_default : t option ref
    val horizontal_default : t option ref
    val set_dim_defaults : t option -> unit
    end

  val pp_mat_gen :
    ?pp_open : (formatter -> unit) ->
    ?pp_close : (formatter -> unit) ->
    ?pp_head : (formatter -> int -> unit) ->
    ?pp_foot : (formatter -> int -> unit) ->
    ?pp_end_row : (formatter -> int -> unit) ->
    ?pp_end_col : (formatter -> row : int -> col : int -> unit) ->
    ?pp_left : (formatter -> int -> unit) ->
    ?pp_right : (formatter -> int -> unit) ->
    ?pad : char option ->
    ?ellipsis : string ->
    ?vertical_context : Context.t option ->
    ?horizontal_context : Context.t option ->
    (formatter -> 'el -> unit) ->
    formatter ->
    ('el, 'a, fortran_layout) Array2.t
    -> unit
  (** [pp_mat_gen
         ?pp_open ?pp_close ?pp_head ?pp_foot ?pp_end_row ?pp_end_col
         ?pp_left ?pp_right ?pad pp_el ppf mat]

      Generic printing of matrices (two-dimensional bigarrays).

      [pp_open ppf] is called whenever printing of a matrix [mat]
      is started, [pp_close ppf] whenever printing is complete.
      These functions are not called when the matrix is empty.

      [pp_head other_ppf col] is used to print a header for column [col]
      in matrix [mat].  This header is right-aligned and eventually padded
      using [Some pad]-character to match the matrix rows in the column
      beneath.  The passed formatter [other_ppf] is not identical to [ppf]!

      [pp_foot other_ppf col] is used to print a footer for column [col]
      in matrix [mat].  It is similar to [pp_head col other_ppf] otherwise.

      [pp_end_row ppf row] is called on row number [row] and formatter
      [ppf] whenever the end of a row has been reached.

      [pp_end_col ppf ~row ~col] is called on the row number [row], column
      number [col] and formatter [ppf] whenever the element at this position
      has been printed and if it is not the last element in the row.

      [pp_left ppf row] is called on row number [row] and formatter
      [ppf] to print labels to the left of each row.  The labels are
      right-aligned within a virtual column.

      [pp_right ppf row] is called on row number [row] and formatter
      [ppf] to print labels to the right of each row.  The labels are
      left-aligned.

      The character [pad] is used to pad matrix elements for right-aligning
      them appropriately.  If it is set to [None], no alignment will
      be performed.

      [ellipsis] is used as a filler when elements need to be skipped in
      the case of printing with contexts.

      [vertical_context] determines the number of initial and final
      rows to be printed.  Intermediate row will be skipped, and one row
      containing ellipsis elements will be printed in their place instead.
      [None] chooses no context, [Some v] sets the vertical context to [v].

      [horizontal_context] determines the number of initial and final
      columns to be printed.  Intermediate columns will be skipped,
      and one columns containing ellipsis elements will be printed in
      their place instead.  [None] chooses no context, [Some h] sets the
      horizontal context to [h].

      [pp_el other_ppf el] is called on formatter [other_ppf] (not
      [ppf]!) and each matrix element.

      [ppf] is the formatter to which all output is finally printed.

      [mat] is the matrix to be printed.

      @param pp_open default = open standard pretty-printing box
      @param pp_close default = close standard pretty-printing box
      @param pp_head default = no default
      @param pp_foot default = no default
      @param pp_end_row default = print newline (within pretty-printing box)
      @param pp_end_col default = print space
      @param pp_left default = no default
      @param pad default = [Some ' ']
      @param ellipsis default = [!Context.ellipsis]
      @param vertical_context default = [Some !Context.vertical_default]
      @param horizontal_context default = [Some !Context.horizontal_default]
  *)


  (** {6 Default pretty-printers used by the other pretty-printing functions} *)

  (** Type of references for default printers of elements *)
  type 'el pp_el_default = (formatter -> 'el -> unit) ref

  val pp_float_el_default : float pp_el_default
  (** fprintf ppf "%G" el *)

  val pp_complex_el_default : Complex.t pp_el_default
  (** fprintf ppf "(%G, %Gi)" el.re el.im *)

  val pp_int32_el : formatter -> int32 -> unit
  (** fprintf ppf "%ld" el *)


  (** {6 Pretty-printing in standard style} *)

  (** Type of standard pretty-printers for column vectors *)
  type ('el, 'elt) pp_vec =
    formatter ->
    ('el, 'elt, fortran_layout) Array1.t
    -> unit
  (** [pp_vec ppf vec] prints a vector [vec] to formatter [ppf]
      using the defaults. *)

  val pp_fvec : (float, 'elt) pp_vec
  val pp_cvec : (Complex.t, 'elt) pp_vec
  val pp_ivec : (int32, 'elt) pp_vec
  val pp_rfvec : (float, 'elt) pp_vec
  val pp_rcvec : (Complex.t, 'elt) pp_vec
  val pp_rivec : (int32, 'elt) pp_vec

  (** Type of standard pretty-printers for matrices *)
  type ('el, 'elt) pp_mat =
    formatter ->
    ('el, 'elt, fortran_layout) Array2.t
    -> unit
  (** [pp_mat ppf mat] prints a matrix [mat] to formatter [ppf] using the
      defaults. *)

  val pp_fmat : (float, 'elt) pp_mat
  val pp_cmat : (Complex.t, 'elt) pp_mat
  val pp_imat : (int32, 'elt) pp_mat


  (** {7 Labeled pretty-printing} *)

  (** {8 Vectors} *)

  (** Type of pretty-printers for labeled vectors *)
  type ('el, 'elt) pp_labeled_vec =
    ?pp_head : (formatter -> int -> unit) ->
    ?pp_foot : (formatter -> int -> unit) ->
    ?pp_left : (formatter -> int -> unit) option ->
    ?pp_right : (formatter -> int -> unit) ->
    ?pad : char option ->
    ?ellipsis : string ->
    ?vertical_context : Context.t option ->
    ?horizontal_context : Context.t option ->
    unit ->
    formatter ->
    ('el, 'elt, fortran_layout) Array1.t
    -> unit
  (** [pp_labeled_vec ?pp_head ?pp_foot ?pp_left ?pp_right ?pad
        ?ellipsis ?vertical_context ?horizontal_context () ppf vec]
      prints vector [vec] to formatter [ppf] labeling the header using
      function [pp_head], the footer using [pp_foot], the left side (of
      rows for column vectors; of columns for row vectors) using [pp_left],
      and the right side using [pp_right].  A [pad]-option and context
      options can be passed.

      For column vectors the labels on the left side are right-aligned
      while those on the right side are left-aligned.

      @param pp_head default = no default (= no printing)
      @param pp_foot default = no default (= no printing)
      @param pp_left default = [Some pp_int32_el] for vector rows/cols
                               (= not in header/footer row/col)
      @param pp_right default = no default (= no printing)
  *)

  val pp_labeled_fvec : (float, 'elt) pp_labeled_vec
  val pp_labeled_cvec : (Complex.t, 'elt) pp_labeled_vec
  val pp_labeled_ivec : (int32, 'elt) pp_labeled_vec
  val pp_labeled_rfvec : (float, 'elt) pp_labeled_vec
  val pp_labeled_rcvec : (Complex.t, 'elt) pp_labeled_vec
  val pp_labeled_rivec : (int32, 'elt) pp_labeled_vec

  (** Type of pretty-printers for string labeled vectors *)
  type ('el, 'elt) pp_lvec =
    ?print_head : bool ->
    ?print_foot : bool ->
    ?print_left : bool ->
    ?print_right : bool ->
    ?labels : string array ->
    ?name : string ->
    ?pad : char option ->
    ?ellipsis : string ->
    ?vertical_context : Context.t option ->
    ?horizontal_context : Context.t option ->
    unit ->
    formatter ->
    ('el, 'elt, fortran_layout) Array1.t
    -> unit
  (** [pp_lvec ?print_head ?print_foot ?print_left ?print_right
        ?labels ?name ?pad ?ellipsis ?vertical_context ?horizontal_context
        () ppf vec]
      prints vector [vec] to formatter [ppf] labeling the header with [name]
      if provided and if [print_head] is true, and labeling the footer with
      [name] if [print_foot] is true.  The left side (of rows for column
      vectors; of columns for row vectors) is labeled with [labels] if
      provided and if [print_left] is true, and the right side is labeled
      with [labels] if [print_right] is true.  A [pad]-option and context
      options can be passed.

      For columns vectors the labels on the left side are right-aligned
      while those on the right side are left-aligned.

      It is the duty of the user to make sure that the array containing
      the labels is sufficiently large for the given vector.

      @param print_head default = [true]
      @param print_foot default = [true]
      @param print_left default = [true]
      @param print_right default = [false]
      @param labels default = no default (= no printing)
      @param header default = no default (= no printing)
  *)

  val pp_lfvec : (float, 'elt) pp_lvec
  val pp_lcvec : (Complex.t, 'elt) pp_lvec
  val pp_livec : (int32, 'elt) pp_lvec
  val pp_rlfvec : (float, 'elt) pp_lvec
  val pp_rlcvec : (Complex.t, 'elt) pp_lvec
  val pp_rlivec : (int32, 'elt) pp_lvec


  (** {8 Matrices} *)

  (** Type of pretty-printers for labeled matrices *)
  type ('el, 'elt) pp_labeled_mat =
    ?pp_head : (formatter -> int -> unit) option ->
    ?pp_foot : (formatter -> int -> unit) option ->
    ?pp_left : (formatter -> int -> unit) option ->
    ?pp_right : (formatter -> int -> unit) option ->
    ?pad : char option ->
    ?ellipsis : string ->
    ?vertical_context : Context.t option ->
    ?horizontal_context : Context.t option ->
    unit ->
    formatter ->
    ('el, 'elt, fortran_layout) Array2.t
    -> unit
  (** [pp_labeled_mat ?pp_head ?pp_foot ?pp_left ?pp_right ?pad
        ?ellipsis ?vertical_context ?horizontal_context () ppf mat]
      prints a matrix [mat] to formatter [ppf] labeling the header using
      function [pp_head], the footer using [pp_foot], the left side of rows
      using [pp_left], and the right one using [pp_right].  A [pad]-option
      and context options can be passed.

      If [None] is passed as argument for the default printers, the
      corresponding labels will not be printed.

      @param pp_head default = [Some pp_int32_el]
      @param pp_foot default = [Some pp_int32_el]
      @param pp_left default = [Some pp_int32_el] for matrix rows
                               (= not in header/footer row)
      @param pp_right default = [Some pp_int32_el] for matrix rows
                               (= not in header/footer row)
  *)

  val pp_labeled_fmat : (float, 'elt) pp_labeled_mat
  val pp_labeled_cmat : (Complex.t, 'elt) pp_labeled_mat
  val pp_labeled_imat : (int32, 'elt) pp_labeled_mat

  (** Type of pretty-printers for string labeled matrices *)
  type ('el, 'elt) pp_lmat =
    ?print_head : bool ->
    ?print_foot : bool ->
    ?print_left : bool ->
    ?print_right : bool ->
    ?row_labels : string array ->
    ?col_labels : string array ->
    ?pad : char option ->
    ?ellipsis : string ->
    ?vertical_context : Context.t option ->
    ?horizontal_context : Context.t option ->
    unit ->
    formatter ->
    ('el, 'elt, fortran_layout) Array2.t
    -> unit
  (** [pp_lmat ?print_head ?print_foot ?print_left ?print_right
        ?row_labels ?col_labels ?pad ?ellipsis
        ?vertical_context ?horizontal_context () ppf mat]
      prints a matrix [mat] to formatter [ppf] labeling the header with
      the column labels in [col_labels] if provided and if [print_head] is
      true, and labeling the footer with the column labels if [print_foot]
      is true.  The left side of rows is labeled with the row labels
      [row_labels] if provided and if [print_left] is true, and the right
      side of rows is labeled with the row labels if [print_right] is true.
      A [pad]-option and context options can be passed.

      It is the duty of the user to make sure that the arrays containing the
      row- and column labels are sufficiently large for the given matrix.

      @param print_head default = [true]
      @param print_foot default = [true]
      @param print_left default = [true]
      @param print_right default = [true]
      @param row_labels default = no default (= no printing)
      @param col_labels default = no default (= no printing)
  *)

  val pp_lfmat : (float, 'elt) pp_lmat
  val pp_lcmat : (Complex.t, 'elt) pp_lmat
  val pp_limat : (int32, 'elt) pp_lmat


  (** {6 Pretty-printing in OCaml-style} *)

  (** Type of pretty-printers for OCaml-vectors *)
  type ('el, 'elt) pp_el_ovec =
    formatter ->
    (formatter -> 'el -> unit) ->
    ('el, 'elt, fortran_layout) Array1.t
    -> unit
  (** [pp_el_ovec ppf pp_el vec] prints the vector [vec] to formatter
      [ppf] in OCaml-style using the element printer [pp_el]. *)

  val pp_ovec : ('el, 'elt) pp_el_ovec
  (** [pp_ovec ppf pp_el vec] prints the column vector [vec] to formatter
      [ppf] in OCaml-style using the element printer [pp_el]. *)

  val pp_rovec : ('el, 'elt) pp_el_ovec
  (** [pp_rovec ppf pp_el vec] prints the row vector [vec] to formatter
      [ppf] in OCaml-style using the element printer [pp_el]. *)

  (** Type of pretty-printers for OCaml-vectors of a given element type *)
  type ('el, 'elt) pp_ovec =
    formatter ->
    ('el, 'elt, fortran_layout) Array1.t
    -> unit
  (** [pp_ovec ppf vec] prints the vector [vec] to formatter [ppf] in
      OCaml-style. *)

  val pp_ofvec : (float, 'elt) pp_ovec
  val pp_ocvec : (Complex.t, 'elt) pp_ovec
  val pp_oivec : (int32, 'elt) pp_ovec

  val pp_rofvec : (float, 'elt) pp_ovec
  val pp_rocvec : (Complex.t, 'elt) pp_ovec
  val pp_roivec : (int32, 'elt) pp_ovec

  val pp_omat :
    formatter ->
    (formatter -> 'el -> unit) ->
    ('el, 'c, fortran_layout) Array2.t
    -> unit
  (** [pp_omat ppf pp_el mat] prints matrix [mat] to formatter [ppf]
      in OCaml-style using the element printer [pp_el]. *)

  (** Type of pretty-printers for OCaml-matrices of a given element type *)
  type ('el, 'elt) pp_omat =
    formatter ->
    ('el, 'elt, fortran_layout) Array2.t
    -> unit
  (** [pp_omat ppf mat] prints the matrix [mat] to formatter [ppf] in
      OCaml-style. *)

  val pp_ofmat : (float, 'elt) pp_omat
  val pp_ocmat : (Complex.t, 'elt) pp_omat
  val pp_oimat : (int32, 'elt) pp_omat


  (** {6 Good pretty-printers for toplevels} *)

  (** These pretty-printers will use index labels for easier identification
      of rows and columns. *)

  module Toplevel : sig
    val lsc : int -> unit
    val pp_fvec : (float, 'elt) pp_vec
    val pp_cvec : (Complex.t, 'elt) pp_vec
    val pp_ivec : (int32, 'elt) pp_vec
    val pp_rfvec : (float, 'elt) pp_vec
    val pp_rcvec : (Complex.t, 'elt) pp_vec
    val pp_rivec : (int32, 'elt) pp_vec

    val pp_fmat : (float, 'elt) pp_mat
    val pp_cmat : (Complex.t, 'elt) pp_mat
    val pp_imat : (int32, 'elt) pp_mat
  end

end

(** Pretty printing of real vector and matrices.  See the
    {!Lacaml.Io} module for more versatile functions. *)
module Real_io : sig
  val pp_num : Format.formatter -> float -> unit
  (** [pp_num ppf el] is equivalent to [fprintf ppf "%G" el]. *)

  val pp_vec : (float, 'a) Io.pp_vec
  (** Pretty-printer for column vectors. *)

  val pp_mat : (float, 'a) Io.pp_mat
  (** Pretty-printer for matrices. *)

end

(** Pretty printing of complex vector and matrices.  See the
    {!Lacaml.Io} module for more versatile functions. *)
module Complex_io : sig
  val pp_num : Format.formatter -> Complex.t -> unit
  (** [pp_num ppf el] is equivalent to [fprintf ppf "(%G, %Gi)"
      el.re el.im]. *)

  val pp_vec : (Complex.t, 'a) Io.pp_vec
  (** Pretty-printer for column vectors. *)

  val pp_mat : (Complex.t, 'a) Io.pp_mat
  (** Pretty-printer for matrices. *)

end


(** {2 Precision dependent modules} *)

(** Types and functions common to all precision dependent sub-modules. *)
module Common : sig

  type trans2 = [ `N | `T ]
  (** Transpose parameter (normal or transposed) *)

  type side = [ `L | `R ]
  (** Side parameter (left or right) *)

  type diag = [ `U | `N ]
  (** Diagonal parameter (unit or non-unit) *)

  type norm2 = [ `O | `I ]
  (** Type of 1-norm ([`O]) and infinity norm ([`I]) *)

  type norm4 = [ norm2 | `M | `F ]
  (** Type of 1-norm ([`O]), infinity norm ([`I]) and the Frobenius norm ([`F]).
      [`M] is the maximum of the absolute values (not a true matrix norm). *)

  type svd_job = [ `A | `S | `O | `N ]
  (** SVD computation flags *)

  exception InternalError of string
  (** [InternalError msg] gets raised when BLAS or LAPACK exhibit undefined
      behaviour. *)

  type int_vec = (int32, int32_elt, fortran_layout) Array1.t
  (** Type of 32bit Fortran integer vectors. *)

  val create_int_vec : int -> int_vec
  (** [create_int_vec n] @return an int-vector with [n] rows. *)

  val mat_from_vec : ('a, 'b, 'c) Array1.t -> ('a, 'b, 'c) Array2.t
  (** [mat_from_vec a] converts the vector [a] into a matrix with [Array1.dim a]
      rows and 1 column.  The data is shared between the two matrices. *)

end

(** Double precision real BLAS and LAPACK functions. *)
module D : sig

  type prec = float64_elt
  type num_type = float

  type vec = (float, float64_elt, fortran_layout) Array1.t
  (** Vectors (precision: float64). *)

  type rvec = vec

  type mat = (float, float64_elt, fortran_layout) Array2.t
  (** Matrices (precision: float64). *)

  type trans3 = [ `N | `T ]

  val prec : (float, float64_elt) Bigarray.kind
  (** Precision for this submodule {!D}.  Allows to write precision
      independent code. *)

  module Vec : sig
    (** {5 Vector operations} *)

    (** {6 Creation of vectors} *)

    val random :
      ?rnd_state : Random.State.t ->
      ?from : float -> ?range : float ->
      int
      -> vec
    (** [random ?rnd_state ?from ?range n] @return a vector
        of size [n] initialized with random elements sampled uniformly from
        [range] starting at [from].  A random state [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param from default = -1.0
        @param range default = 2.0 *)

    val sqr :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [sqr ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the square
        of [n] elements of the vector [x] using [incx] as incremental
        steps.   If [y] is given, the result will be stored in there
        using increments of [incy], otherwise a fresh vector will be
        used.  The resulting vector is returned.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1
    *)

    val sqrt :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [sqrt ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the square root
        of [n] elements of the vector [x] using [incx] as incremental
        steps.   If [y] is given, the result will be stored in there
        using increments of [incy], otherwise a fresh vector will be
        used.  The resulting vector is returned.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1
    *)

    (** {6 Creation/conversion of vectors and dimension accessor} *)

    val create : int -> vec
    (** [create n] @return a vector with [n] rows (not initialized). *)

    val make : int -> num_type -> vec
    (** [make n x] @return a vector with [n] rows initialized with value [x]. *)

    val make0 : int -> vec
    (** [make0 n x] @return a vector with [n] rows initialized with the zero
        element. *)

    val init : int -> (int -> num_type) -> vec
    (** [init n f] @return a vector containing [n] elements, where each
        element at position [i] is initialized by the result of calling
        [f i]. *)

    val of_array : num_type array -> vec
    (** [of_array ar] @return a vector initialized from array [ar]. *)

    val to_array : vec -> num_type array
    (** [to_array v] @return an array initialized from vector [v]. *)

    val of_list : num_type list -> vec
    (** [of_list l] @return a vector initialized from list [l]. *)

    val to_list : vec -> num_type list
    (** [to_list v] @return a list initialized from vector [v]. *)

    val append : vec -> vec -> vec
    (** [append v1 v2] @return the vector resulting from appending vector
        [v2] to [v1]. *)

    val concat : vec list -> vec
    (** [concat vs] @return the concatenation of vectors [vs]. *)

    val empty : vec
    (** [empty], the empty vector. *)

    val linspace : ?y : vec -> num_type -> num_type -> int -> vec
    (** [linspace ?z a b n] @return the vector [y] overwritten with [n]
        linearly spaced points between and including [a] and [b].
        @param y default = fresh vector of dim [n] *)

    val logspace : ?y : vec -> num_type -> num_type -> ?base : float -> int -> vec
    (** [logspace ?z a b base n] @return the vector [y] overwritten with [n]
        points logarithmically spaced using base [b] between and including
        [base] ** [a] and [base] ** [b].
        @param y default = fresh vector of dim [n]
        @param base default = 10.0 *)

    val dim : vec -> int
    (** [dim x] @return the dimension of vector [x]. *)


    (** {6 Iterators over vectors} *)

    val map :
      (num_type -> num_type) ->
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [map f ?n ?ofsx ?incx x] @return a new vector resulting from the
        application of [f] to each element of [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val iter :
      (num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iter ?n ?ofsx ?incx f x] applies function [f] in turn to all elements
        of vector [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val iteri :
      (int -> num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iteri ?n ?ofsx ?incx f x] same as [iter] but additionally passes
        the index of the element as first argument and the element itself
        as second argument. *)

    val fold :
      ('a -> num_type -> 'a) ->
      'a ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> 'a
    (** [fold f a ?n ?ofsx ?incx x] is
        [f (... (f (f a x.{ofsx}) x.{ofsx + incx}) ...) x.{ofsx + (n-1)*incx}]
        if [incx > 0] and the same in the reverse order of appearance of the
        [x] values if [incx < 0].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)


    (** {6 Operations on one vector} *)

    val rev : vec -> vec
    (** [rev x] reverses vector [x] (non-destructive). *)

    val max : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [max ?n ?ofsx ?incx x] computes the greater of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps. NaNs
        are ignored. If only NaNs are encountered, the negative [infinity]
        value will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val min : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [min ?n ?ofsx ?incx x] computes the smaller of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps.
        NaNs are ignored. If only NaNs are encountered, the [infinity] value
        will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [sum ?n ?ofsx ?incx x] computes the sum of the [n] elements in
        vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val prod : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [prod ?n ?ofsx ?incx x] computes the product of the [n] elements
        in vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sqr_nrm2 :
      ?stable : bool -> ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
    (** [sqr_nrm2 ?stable ?n ?c ?ofsx ?incx x] computes the square of
        the 2-norm (Euclidean norm) of vector [x] separated by [incx]
        incremental steps.  If [stable] is true, this is equivalent to
        squaring the result of calling the BLAS-function [nrm2], which
        avoids over- and underflow if possible.  If [stable] is false
        (default), [dot] will be called instead for greatly improved
        performance.

        @param stable default = [false]
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
    *)

    val ssqr :
      ?n : int ->
      ?c : num_type ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> num_type
    (** [ssqr ?n ?c ?ofsx ?incx x] computes the sum of squared differences
        of the [n] elements in vector [x] from constant [c], separated
        by [incx] incremental steps.  Please do not confuse with
        {!sqr_nrm2}!  The current function behaves differently with
        complex numbers when zero is passed in for [c].  It computes
        the square for each entry then, whereas {!sqr_nrm2} uses the
        conjugate transpose in the product.  The latter will therefore
        always return a real number.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param c default = zero
        @param ofsx default = 1
        @param incx default = 1
    *)


    (** {6 Operations on two vectors} *)

    val neg :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [neg ?n ?ofsy ?incy ?y ?ofsx ?incx x] negates [n] elements of the
        vector [x] using [incx] as incremental steps.   If [y] is given,
        the result will be stored in there using increments of [incy],
        otherwise a fresh vector will be used.  The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val add :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] adds [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val sub :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] subtracts [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val mul :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] multiplies
        [n] elements of vectors [x] and [y] elementwise, using [incx]
        and [incy] as incremental steps respectively. If [z] is given, the
        result will be stored in there using increments of [incz], otherwise
        a fresh vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val div :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] divides [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val ssqr_diff :
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> num_type
    (** [ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y] returns the sum of
        squared differences of [n] elements of vectors [x] and [y], using
        [incx] and [incy] as incremental steps respectively.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

  end

  module Mat : sig
    (** {5 Matrix operations} *)

    (** {6 Creation of matrices} *)

    val hilbert : int -> mat
    (** [hilbert n] @return an [n]x[n] Hilbert matrix. *)

    val hankel : int -> mat
    (** [hankel n] @return an [n]x[n] Hankel matrix. *)

    val pascal : int -> mat
    (** [pascal n] @return an [n]x[n] Pascal matrix. *)

    val rosser : unit -> mat
    (** [rosser n] @return 8x8 Rosser matrix. *)

    val toeplitz : vec -> mat
    (** [toeplitz v] @return the Toeplitz matrix associated with [v].
        The constant diagonals are read from left to right from [v].
        @raise Invalid_argument if the length of [v] is not an odd number. *)

    val vandermonde : vec -> mat
    (** [vandermonde v] @return the Vandermonde matrix associated with [v]. *)

    val wilkinson : int -> mat
    (** [wilkinson n] @return the [n]x[n] Wilkinson matrix.
        @raise Invalid_argument if [n] is not an odd number >= 3. *)

    val random :
      ?rnd_state : Random.State.t ->
      ?from : float -> ?range : float ->
      int -> int
      -> mat
    (** [random ?rnd_state ?from ?range m n] @return an [m]x[n] matrix
        initialized with random elements sampled uniformly from [range]
        starting at [from].  A random state [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param from default = -1.0
        @param range default = 2.0 *)

    open Common
    (** {6 Creation of matrices and accessors} *)

    val create : int -> int -> mat
    (** [create m n] @return a matrix containing [m] rows and [n] columns. *)

    val make : int -> int -> num_type -> mat
    (** [make m n x] @return a matrix containing [m] rows and [n] columns
        initialized with value [x]. *)

    val make0 : int -> int -> mat
    (** [make0 m n x] @return a matrix containing [m] rows and [n] columns
        initialized with the zero element. *)

    val of_array : num_type array array -> mat
    (** [of_array ar] @return a matrix initialized from the array of arrays
        [ar].  It is assumed that the OCaml matrix is in row major order
        (standard). *)

    val to_array : mat -> num_type array array
    (** [to_array mat] @return an array of arrays initialized from matrix
        [mat]. *)

    val of_col_vecs : vec array -> mat
    (** [of_col_vecs ar] @return a matrix whose columns are initialized from
        the array of vectors [ar].  The vectors must be of same length. *)

    val to_col_vecs : mat -> vec array
    (** [to_col_vecs mat] @return an array of column vectors initialized
        from matrix [mat]. *)

    val as_vec : mat -> vec
    (** [as_vec mat] @return a vector containing all elements of the
        matrix in column-major order.  The data is shared. *)

    val init_rows : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed row-wise. *)

    val init_cols : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed column-wise. *)

    val create_mvec : int -> mat
    (** [create_mvec m] @return a matrix with one column containing [m] rows. *)

    val make_mvec : int -> num_type -> mat
    (** [make_mvec m x] @return a matrix with one column containing [m] rows
        initialized with value [x]. *)

    val mvec_of_array : num_type array -> mat
    (** [mvec_of_array ar] @return a matrix with one column
        initialized with values from array [ar]. *)

    val mvec_to_array : mat -> num_type array
    (** [mvec_to_array mat] @return an array initialized with values from
        the first (not necessarily only) column vector of matrix [mat]. *)

    val from_col_vec : vec -> mat
    (** [from_col_vec v] @return a matrix with one column representing vector [v].
        The data is shared. *)

    val from_row_vec : vec -> mat
    (** [from_row_vec v] @return a matrix with one row representing vector [v].
        The data is shared. *)

    val empty : mat
    (** [empty], the empty matrix. *)

    val identity : int -> mat
    (** [identity n] @return the [n]x[n] identity matrix. *)

    val of_diag : vec -> mat
    (** [of_diag v] @return the diagonal matrix with diagonals elements from [v]. *)

    val dim1 : mat -> int
    (** [dim1 m] @return the first dimension of matrix [m] (number of rows). *)

    val dim2 : mat -> int
    (** [dim2 m] @return the second dimension of matrix [m] (number of columns). *)

    val col : mat -> int -> vec
    (** [col m n] @return the [n]th column of matrix [m] as a vector.
        The data is shared. *)

    val copy_row : ?vec : vec -> mat -> int -> vec
    (** [copy_row ?vec mat int] @return a copy of the [n]th row of matrix [m]
        in vector [vec].

        @param vec default = fresh vector of length [dim2 mat]
    *)


    (** {6 Matrix transformations} *)

    val transpose_copy :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?br : int -> ?bc : int -> mat ->
      unit
    (** [transpose_copy ?m ?n ?ar ?ac a ?br ?bc b] copy the transpose
        of (sub-)matrix [a] into (sub-)matrix [b].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
        @param br default = [1]
        @param bc default = [1]
    *)


    val transpose : ?m : int -> ?n : int -> ?ar : int -> ?ac : int -> mat -> mat
    (** [transpose ?m ?n ?ar ?ac aa] @return the transpose of (sub-)matrix [a].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val detri : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> unit
    (** [detri ?up ?n ?ar ?ac a] takes a triangular (sub-)matrix [a], i.e. one
        where only the upper (iff [up] is true) or lower triangle is defined,
        and makes it a symmetric matrix by mirroring the defined triangle
        along the diagonal.

        @param up default = [true]
        @param n default = [Mat.dim1 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val packed : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> vec
    (** [packed ?up ?n ?ar ?ac a] @return (sub-)matrix [a] in packed
        storage format.

        @param up default = [true]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val unpacked : ?up : bool -> ?n : int -> vec -> mat
    (** [unpacked ?up x] @return an upper or lower (depending on [up])
        triangular matrix from packed representation [vec].  The other
        triangle of the matrix will be filled with zeros.

        @param up default = [true]
        @param n default = [Vec.dim x]
    *)


    (** {6 Arithmetic and other matrix operations} *)

    val copy_diag : mat -> vec
    (** [copy_diag m] @return the diagonal of matrix [m] as a vector.
        If [m] is not a square matrix, the longest possible sequence
        of diagonal elements will be returned. *)

    val trace : mat -> num_type
    (** [trace m] @return the trace of matrix [m].  If [m] is not a
        square matrix, the sum of the longest possible sequence of
        diagonal elements will be returned. *)

    val scal :
      ?m : int -> ?n : int -> num_type -> ?ar : int -> ?ac : int -> mat -> unit
    (** [scal ?m ?n alpha ?ar ?ac a] BLAS [scal] function for (sub-)matrices. *)

    val scal_cols :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?ofs : int -> vec ->
      unit
    (** [scal_cols ?m ?n ?ar ?ac a ?ofs alphas] column-wise [scal]
        function for matrices. *)

    val scal_rows :
      ?m : int -> ?n : int ->
      ?ofs : int -> vec ->
      ?ar : int -> ?ac : int -> mat ->
      unit
    (** [scal_rows ?m ?n ?ofs alphas ?ar ?ac a] row-wise [scal]
        function for matrices. *)

    val axpy :
      ?m : int ->
      ?n : int ->
      ?alpha : num_type ->
      ?xr : int ->
      ?xc : int ->
      x : mat ->
      ?yr : int ->
      ?yc : int ->
      mat
      -> unit
    (** [axpy ?m ?n ?alpha ?xr ?xc ~x ?yr ?yc y] BLAS [axpy] function for
        matrices. *)

    val gemm_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?transa : trans3 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      vec
    (** [gemm_diag ?n ?k ?beta ?ofsy ?y ?transa ?transb ?alpha ?ar ?ac a ?br ?bc b]
        computes the diagonal of the product of the (sub-)matrices [a]
        and [b] (taking into account potential transposing), multiplying
        it with [alpha] and adding [beta] times [y], storing the result in
        [y] starting at the specified offset.  [n] elements of the diagonal
        will be computed, and [k] elements of the matrices will be part of
        the dot product associated with each diagonal element.

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param transa default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?trans : trans2 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      vec
    (** [syrk_diag ?n ?k ?beta ?ofsy ?y ?trans ?alpha ?ar ?ac a]
        computes the diagonal of the symmetric rank-k product of the
        (sub-)matrix [a], multiplying it with [alpha] and adding [beta]
        times [y], storing the result in [y] starting at the specified
        offset.  [n] elements of the diagonal will be computed, and [k]
        elements of the matrix will be part of the dot product associated
        with each diagonal element.

        @param n default = number of rows of [a] (or tr[a])
        @param k default = number of columns of [a] (or tr[a])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param trans default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val gemm_trace :
      ?n : int ->
      ?k : int ->
      ?transa : trans3 ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [gemm_trace ?n ?k ?transa ?ar ?ac a ?transb ?br ?bc b] computes
        the trace of the product of the (sub-)matrices [a] and [b]
        (taking into account potential transposing).  [n] is the number
        of rows (columns) to consider in [a], and [k] the number of
        columns (rows) in [b].

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param transa default = [`N]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_trace :
      ?n : int ->
      ?k : int ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      num_type
    (** [syrk_trace ?n ?k ?ar ?ac a] computes the trace of either [a' * a]
        or [a * a'], whichever is more efficient (results are identical),
        of the (sub-)matrix [a] multiplied by its own transpose.  [n]
        is the number of rows to consider in [a], and [k] the number
        of columns to consider.

        @param n default = number of rows of [a]
        @param k default = number of columns of [a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val symm2_trace :
      ?n : int ->
      ?upa : bool ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?upb : bool ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [symm2_trace ?n ?upa ?ar ?ac a ?upb ?br ?bc b] computes the
        trace of the product of the symmetric (sub-)matrices [a] and
        [b].  [n] is the number of rows and columns to consider in [a]
        and [b].

        @param n default = dimensions of [a] and [b]
        @param upa default = true (upper triangular portion of [a] is accessed)
        @param ar default = [1]
        @param ac default = [1]
        @param upb default = true (upper triangular portion of [b] is accessed)
        @param br default = [1]
        @param bc default = [1]
    *)


    (** {6 Iterators over matrices} *)

    val map :
      (num_type -> num_type) ->
      ?m : int ->
      ?n : int ->
      ?br : int ->
      ?bc : int ->
      ?b : mat ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> mat
    (** [map f ?m ?n ?br ?bc ?b ?ar ?ac a]
        @return matrix with [f] applied to each element of [a].
        @param m default = number of rows of [a]
        @param n default = number of columns of [a]
        @param b default = fresh matrix of size m by n *)

    val fold_cols : ('a -> vec -> 'a) -> ?n : int -> ?ac : int -> 'a -> mat -> 'a
    (** [fold_cols f ?n ?ac acc a]
        @return accumulator resulting from folding over each column vector.
        @param ac default = 1
        @param n default = number of columns of [a] *)

  end

  val pp_num : Format.formatter -> float -> unit
  (** [pp_num ppf el] is equivalent to [fprintf ppf "%G" el]. *)

  val pp_vec : (float, 'a) Io.pp_vec
  (** Pretty-printer for column vectors. *)

  val pp_mat : (float, 'a) Io.pp_mat
  (** Pretty-printer for matrices. *)


  open Common
  (** {6 BLAS-1 interface} *)

  val dot :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> float
  (** [dot ?n ?ofsy ?incy y ?ofsx ?incx x] see BLAS documentation!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param ofsx default = 1
      @param incx default = 1
  *)

  val asum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
  (** [asum ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-2 interface} *)

  val sbmv :
    ?n : int ->
    ?k : int ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?up : bool ->
    ?alpha : float ->
    ?beta : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> vec
  (** [sbmv ?n ?k ?ofsy ?incy ?y ?ar ?ac a ?up ?alpha ?beta ?ofsx ?incx x] see
      BLAS documentation!

      @return vector [y], which is overwritten.

      @param n default = number of available columns to the right of [ac].
      @param k default = number of available rows in matrix [a] - 1
      @param ofsy default = 1
      @param incy default = 1
      @param ar default = 1
      @param ac default = 1
      @param y default = uninitialized vector of minimal length (see BLAS)
      @param up default = true i.e., upper band of [a] is supplied
      @param alpha default = 1.0
      @param beta default = 0.0
      @param ofsx default = 1
      @param incx default = 1
  *)

  val ger :
    ?m : int ->
    ?n : int ->
    ?alpha : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> mat
  (** [ger ?m ?n ?alpha ?ofsx ?incx x ?ofsy ?incy y n ?ar ?ac a] see
      BLAS documentation!

      @return vector [a], which is overwritten

      @param m default = number of rows of [a]
      @param n default = number of columns of [a]
      @param alpha default = 1.0
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  val syr :
    ?n : int ->
    ?alpha : float ->
    ?up : bool ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> mat
  (** [syr ?n ?alpha ?up ?ofsx ?incx x ?ar ?ac a] see BLAS documentation!

      @return matrix [a], which is overwritten

      @param n default = number of rows of [a]
      @param alpha default = 1.0
      @param up default = true i.e., upper triangle of [a] is supplied
      @param ofsx default = 1
      @param incx default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  (** {6 LAPACK interface} *)

  (** {7 Auxiliary routines} *)

  val lansy_min_lwork : int -> norm4 -> int
  (** [lansy_min_lwork m norm]
      @return the minimum length of the work array used by the [lansy]-function.
      @param norm type of norm that will be computed by [lansy]
      @param n the number of columns (and rows) in the matrix *)

  val lansy :
    ?n : int ->
    ?up : bool ->
    ?norm : norm4 ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
  (** [lansy ?norm ?up ?n ?ar ?ac ?work a] see LAPACK documentation!
      @param norm default = `O
      @param up default = true (reference upper triangular part of [a])
      @param n default = number of columns of matrix [a]
      @param work default = allocated work space for norm `I *)

  val lamch :  [ `E | `S | `B | `P | `N | `R | `M | `U | `L | `O ] -> float
  (** [lamch cmach] see LAPACK documentation! *)


  (** {7 Linear equations (computational routines)} *)

  val orgqr_min_lwork : n : int -> int
  (** [orgqr_min_lwork ~n] @return the minimum length of the
      work-array used by the [orgqr]-function if the matrix has [n]
      columns. *)

  val orgqr_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [orgqr_opt_lwork ?m ?n ?k ~tau ?ar ?ac a] @return the optimum
      length of the work-array used by the [orgqr_opt_lwork]-function
      given matrix [a], optionally its logical dimensions [m] and
      [n], and the number of reflectors [k].

      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param k default = available number of elements in vector [tau]
  *)

  val orgqr :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?work : vec ->
    tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [orgqr ?m ?n ?k ?work ~tau ?ar ?ac a] see LAPACK documentation!

      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param k default = available number of elements in vector [tau]
  *)


  val ormqr :
    ?side : side ->
    ?trans : trans2 ->
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?work : vec ->
    tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?cr : int ->
    ?cc : int ->
    mat ->
    unit
  (** [ormqr ?side ?trans ?m ?n ?k ?work ~tau ?ar ?ac a ?cr ?cc c]
      see LAPACK documentation!

      @param side default = [`L]
      @param trans default = [`N]
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param k default = available number of elements in vector [tau]
  *)


  val gecon_min_lwork : int -> int
  (** [gecon_min_lwork n] @return the minimum length of the work array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to
               the [gecon]-function *)

  val gecon_min_liwork : int -> int
  (** [gecon_min_liwork n] @return the minimum length of the iwork array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to [gecon]-function *)

  val gecon :
    ?n : int ->
    ?norm : norm2 ->
    ?anorm : float ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
  (** [gecon ?n ?norm ?anorm ?work ?rwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of matrix [a]
      @param n default = available number of columns of matrix [a]
      @param norm default = 1-norm
      @param anorm default = norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace
      @param iwork default = automatically allocated workspace
      @param ar default = 1
      @param ac default = 1 *)

  val sycon_min_lwork : int -> int
  (** [sycon_min_lwork n] @return the minimum length of the work array
      used by the [sycon]-function.
      @param n the logical dimensions of the matrix given to
               the [sycon]-function *)

  val sycon_min_liwork : int -> int
  (** [sycon_min_liwork n] @return the minimum length of the iwork array
      used by the [sycon]-function.
      @param n the logical dimensions of the matrix given to [sycon]-function *)

  val sycon :
      ?n : int ->
      ?up : bool ->
      ?ipiv : int_vec ->
      ?anorm : float ->
      ?work : vec ->
      ?iwork : int_vec ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> float
  (** [sycon ?n ?up ?ipiv ?anorm ?work ?iwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number
              of symmetric matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of the factorization of [a] is stored
      @param ipiv default = vec of length [n]
      @param anorm default = 1-norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace
      @param iwork default = automatically allocated workspace *)

  val pocon_min_lwork : int -> int
  (** [pocon_min_lwork n] @return the minimum length of the work array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to
               the [pocon]-function *)

  val pocon_min_liwork : int -> int
  (** [pocon_min_liwork n] @return the minimum length of the iwork array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to [pocon]-function *)

  val pocon :
      ?n : int ->
      ?up : bool ->
      ?anorm : float ->
      ?work : vec ->
      ?iwork : int_vec ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> float
  (** [pocon ?n ?up ?anorm ?work ?iwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of
              symmetric positive definite matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of Cholesky factorization
                          of [a] is stored
      @param work default = automatically allocated workspace
      @param iwork default = automatically allocated workspace
      @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)

  (** {7 Least squares (expert drivers)} *)

  val gelsy_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gelsy_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gelsy]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gelsy_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsy_opt_lwork ?m ?n ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [gelsy]-function given matrix
      [a], optionally its logical dimensions [m] and [n] and given right
      hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param nrhs default = available number of columns in matrix [b] *)

  val gelsy :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?rcond : float ->
    ?jpvt : int_vec ->
    ?work : vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsy ?m ?n ?ar ?ac a ?rcond ?jpvt ?ofswork ?work ?nrhs b] see LAPACK
      documentation!  @return the effective rank of [a].
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param rcond default = (-1) => machine precision
      @param jpvt default = vec of length [n]
      @param work default = vec of optimum length (-> [gelsy_opt_lwork])
      @param nrhs default = available number of columns in matrix [b] *)

  val gelsd_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gelsd_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gelsd]-function if the logical dimensions
      of the matrix are [m] and [n] and if there are [nrhs] right hand
      side vectors. *)

  val gelsd_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsd_opt_lwork ?m ?n ?ar ?ac a ?nrhs b] @return the optimum length of
      the work-array used by the [gelsd]-function given matrix [a],
      optionally its logical dimensions [m] and [n] and given right hand
      side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param nrhs default = available number of columns in matrix [b] *)

  val gelsd_min_iwork : int -> int -> int
  (** [gelsd_min_iwork m n] @return the minimum (= optimum) length
      of the iwork-array used by the [gelsd]-function if the logical
      dimensions of the matrix are [m] and [n]. *)

  val gelsd :
    ?m : int ->
    ?n : int ->
    ?rcond : float ->
    ?ofss : int ->
    ?s : vec ->
    ?work : vec ->
    ?iwork : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsd ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs b]
      see LAPACK documentation!
      @return the effective rank of [a].
      @raise Failure if the function fails to converge.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param rcond default = (-1) => machine precision
      @param ofss default = 1 or ignored if [s] is not given
      @param s default = vec of length [min rows cols]
      @param work default = vec of optimum length (-> [gelsd_opt_lwork])
      @param iwork default = vec of optimum (= minimum) length
      @param nrhs default = available number of columns in matrix [b] *)

  val gelss_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gelss_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gelss]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gelss_opt_lwork :
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?m : int ->
    ?n : int ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelss_opt_lwork ?ar ?ac a ?m ?n ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [gelss]-function given matrix
      [a], optionally its logical dimensions [m] and [n] and given right
      hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param nrhs default = available number of columns in matrix [b] *)

  val gelss :
    ?m : int ->
    ?n : int ->
    ?rcond : float ->
    ?ofss : int ->
    ?s : vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelss ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs ?br ?bc b]
      see LAPACK documentation!
      @return the effective rank of [a].
      @raise Failure if the function fails to converge.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param rcond default = (-1) => machine precision
      @param ofss default = 1 or ignored if [s] is not given
      @param s default = vec of length [min m n]
      @param work default = vec of optimum length (-> [gelss_opt_lwork])
      @param nrhs default = available number of columns in matrix [b] *)


  (** {7 General SVD routines} *)

  val gesvd_min_lwork : m : int -> n : int -> int
  (** [gesvd_min_lwork ~m ~n] @return the minimum length of the work array
      used by the [gesvd]-function for matrices with [m] rows and [n]
      columns. *)

  val gesvd_opt_lwork :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?ar : int -> ?ac : int -> mat
    -> int

  val gesvd :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?work : vec ->
    ?ar : int -> ?ac : int -> mat
    -> vec * mat * mat

  val gesdd_liwork : m : int -> n : int -> int

  val gesdd_min_lwork : ?jobz : svd_job -> m : int -> n : int -> unit -> int
  (** [gesdd_min_lwork ?jobz ~m ~n] @return the minimum length of the
      work array used by the [gesdd]-function for matrices with [m] rows
      and [n] columns for SVD-job [jobz]. *)

  val gesdd_opt_lwork :
    ?m : int -> ?n : int ->
    ?jobz : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?iwork : int_vec ->
    ?ar : int -> ?ac : int -> mat
    -> int

  val gesdd :
    ?m : int -> ?n : int ->
    ?jobz : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ar : int -> ?ac : int -> mat
    -> vec * mat * mat


  (** {7 General eigenvalue problem (simple drivers)} *)

  val geev_min_lwork : ?vectors : bool -> int -> int
  (** [geev_min_lwork vectors n] @return the minimum length of the
      work array used by the [geev]-function. [vectors] indicates whether
      eigenvectors are supposed to be computed.
      @param n the logical dimensions of the matrix given to [geev]-function
      @param vectors default = true *)

  val geev_opt_lwork :
    ?n : int ->
    ?vlr : int -> ?vlc : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofswr : int -> ?wr : vec ->
    ?ofswi : int -> ?wi : vec ->
    ?ar : int -> ?ac : int -> mat ->
    int
   (** [geev_opt_lwork
         ?n
         ?vlr ?vlc ?vl
         ?vrr ?vrc ?vr
         ?ofswr wr
         ?ofswi wi
         ?ar ?ac a]
      See [geev]-function for details about arguments.
      @return "optimal" size of work array. *)

  val geev :
    ?n : int ->
    ?work : vec ->
    ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofswr : int -> ?wr : vec ->
    ?ofswi : int -> ?wi : vec ->
    ?ar : int -> ?ac : int -> mat ->
    mat * vec * vec * mat
  (** [geev ?work ?n
        ?vlr ?vlc ?vl
        ?vrr ?vrc ?vr
        ?ofswr wr ?ofswi wi
        ?ar ?ac a]
      @return ([lv], [wr], [wi], [rv]), where [wr] and [wv] are the real
        and imaginary components of the eigenvalues, and [lv] and [rv]
        are the left and right eigenvectors. [lv] ([rv]) is the empty
        matrix if [vl] ([vr]) is set to [None].
      @raise Failure if the function fails to converge
      @param n default = available number of columns of matrix [a]
      @param work default = automatically allocated workspace
      @param vl default = Automatically allocated left eigenvectors.
                          Pass [None] if you do not want to compute them,
                          [Some lv] if you want to provide the storage.
                          You can set [vlr], [vlc] in the last case.
      (See LAPACK GEEV docs for details about storage of complex eigenvectors)
      @param vr default = Automatically allocated right eigenvectors.
                          Pass [None] if you do not want to compute them,
                          [Some rv] if you want to provide the storage.
                          You can set [vrr], [vrc] in the last case.
      @param wr default = vector of size [n]; real components of the eigenvalues
      @param wi default = vector of size [n];
                          imaginary components of the eigenvalues
      @param a the matrix whose eigensystem is computed *)


  (** {7 Symmetric-matrix eigenvalue and singular value problems
      (simple drivers)} *)

  val syev_min_lwork : int -> int
  (** [syev_min_lwork n] @return the minimum length of the work-array
      used by the {!syev}-function if the logical dimensions of the matrix
      are [n]. *)

  val syev_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syev_opt_lwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
      length of the work-array used by the {!syev}-function given matrix
      [a], optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syev :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> vec
  (** [syev ?n ?vectors ?up ?ofswork ?work ?ofsw ?w ?ar ?ac a] computes
      all eigenvalues and, optionally, eigenvectors of the real symmetric
      matrix [a].

      @return the vector [w] of eigenvalues in ascending order.
      @raise Failure if the function fails to converge.
      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param up default = true i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length (-> {!syev_opt_lwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n] *)

  val syevd_min_lwork : vectors : bool -> int -> int
  (** [syevd_min_lwork vectors n] @return the minimum length of the
      work-array used by the {!syevd}-function if the logical dimensions of
      the matrix are [n] and given whether eigenvectors should be computed
      ([vectors]). *)

  val syevd_min_liwork : vectors : bool -> int -> int
  (** [syevd_min_liwork vectors n] @return the minimum length of the
      iwork-array used by the {!syevd}-function if the logical dimensions of
      the matrix are [n] and given whether eigenvectors should be computed
      ([vectors]). *)

  val syevd_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevd_opt_lwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
      length of the work-array used by the {!syevd}-function given matrix
      [a], optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevd_opt_liwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevd_opt_liwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
      length of the iwork-array used by the {!syevd}-function given matrix
      [a], optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevd_opt_l_li_work :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int * int
  (** [syevd_opt_l_li_iwork ?n ?vectors ?up ?ar ?ac a] @return the tuple
      of optimum lengths of the work- and iwork-arrays respectively,
      used by the {!syevd}-function given matrix [a], optionally its
      logical dimension [n] and whether the eigenvectors must be computed
      ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevd :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> vec
  (** [syevd ?n ?vectors ?up ?ofswork ?work ?iwork ?ofsw ?w ?ar ?ac a]
      computes all eigenvalues and, optionally, eigenvectors of the real
      symmetric matrix [a].  If eigenvectors are desired, it uses a
      divide and conquer algorithm.

      @return the vector [w] of eigenvalues in ascending order.
      @raise Failure if the function fails to converge.
      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param up default = true i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length (-> {!syev_opt_lwork})
      @param iwork default = int_vec of optimum length (-> {!syevd_opt_liwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n] *)

  val sbev_min_lwork : int -> int
  (** [sbev_min_lwork n] @return the minimum length of the work-array
      used by the {!sbev}-function if the logical dimensions of the matrix
      are [n]. *)

  val sbev :
    ?n : int ->
    ?kd : int ->
    ?zr : int ->
    ?zc : int ->
    ?z : mat ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?abr : int ->
    ?abc : int ->
    mat
    -> vec
  (** [sbev ?n ?vectors ?zr ?zc ?z ?up ?ofswork ?work ?ofsw ?w ?abr ?abc ab]
      computes all the eigenvalues and, optionally, eigenvectors of the
      real symmetric {i band} matrix [ab].

      @raise Failure if the function fails to converge.

      @return the vector [w] of eigenvalues in ascending order.
      @raise Failure if the function fails to converge.
      @param n default = available number of columns of matrix [ab]
      @param z matrix to contain the orthonormal eigenvectors of [ab],
               the [i]-th column of [z] holding the eigenvector associated
               with [w.{i}].
               default = [None] i.e, eigenvectors are not computed
      @param kd default = number of rows in matrix [ab] - 1
      @param up default = true i.e., upper triangle of the matrix is stored
      @param work default = vec of minimal length (-> {!sbev_min_lwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]
      @param abr default = 1
      @param abc default = 1 *)


  (** {7 Symmetric-matrix eigenvalue and singular value problems (expert &
      RRR drivers)} *)

  val syevr_min_lwork : int -> int
  (** [syevr_min_lwork n] @return the minimum length of the
      work-array used by the {!syevr}-function if the logical dimensions
      of the matrix are [n]. *)

  val syevr_min_liwork : int -> int
  (** [syevr_min_liwork n] @return the minimum length of the
      iwork-array used by the {!syevr}-function if the logical dimensions
      of the matrix are [n]. *)

  val syevr_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevr_opt_lwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a] @return
      the optimum length of the work-array used by the {!syevr}-function
      given matrix [a], optionally its logical dimension [n] and whether
      the eigenvectors must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevr_opt_liwork :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevr_opt_liwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a] @return
      the optimum length of the iwork-array used by the {!syevr}-function
      given matrix [a], optionally its logical dimension [n] and whether
      the eigenvectors must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevr_opt_l_li_work :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int * int
  (** [syevr_opt_l_li_iwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a]
      @return the tuple of optimum lengths of the work- and iwork-arrays
      respectively, used by the {!syevr}-function given matrix [a],
      optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevr :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?zr : int ->
    ?zc : int ->
    ?z : mat ->
    ?isuppz : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int * vec * mat * int_vec
  (** [syevr
        ?n ?vectors ?range ?up ?abstol ?work ?iwork
        ?ofsw ?w ?zr ?zc ?z ?isuppz ?ar ?ac a]
      [range] is either [`A] for computing all eigenpairs, [`V (vl, vu)]
      defines the lower and upper range of computed eigenvalues, [`I (il,
      iu)] defines the indexes of the computed eigenpairs, which are sorted
      in ascending order.
      @return the tuple [(m, w, z, isuppz)], where [m] is the number
              of computed eigenpairs, vector [w] contains the computed
              eigenvalues in ascending order, [z] contains the computed
              eigenvectors in same order, and [isuppz] indicates the
              nonzero elements in [z].
      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param range default = `A
      @param up default = true i.e., upper triangle of [a] is stored
      @param abstol default = result of calling [lamch `S]
      @param work default = vec of optimum length (-> {!syev_opt_lwork})
      @param iwork default = int_vec of optimum length (-> {!syevr_opt_liwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]
      @param zr default = 1
      @param zc default = 1
      @param z default = matrix with minimal required dimension
      @param isuppz default = [int_vec] with minimal required dimension
      @param ar default = 1
      @param ac default = 1 *)

  val sygv_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?itype : [`A_B | `AB | `BA ] ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [sygv_opt_lwork ?n ?vectors ?up ?ar ?ac a ?br ?bc b] @return the
      optimum length of the work-array used by the {!sygv}-function
      for the given matrices [a] and [b], optionally their logical
      dimension [n] and whether the eigenvectors must be computed
      ([vectors]).

      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored

      @param itype specifies the problem type to be solved:
             - [`A_B] (default): a*x = (lambda)*a*x
             - [`AB]: a*b*x = (lambda)*x
             - [`BA]: b*a*x = (lambda)*x
  *)

  val sygv :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?itype : [`A_B | `AB | `BA ] ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat
    -> vec
  (** [sygv ?n ?vectors ?up ?ofswork ?work ?ofsw ?w ?ar ?ac a]
      computes all the eigenvalues, and optionally, the eigenvectors
      of a real generalized symmetric-definite eigenproblem, of the
      form [a*x=(lambda)*b*x], [a*b*x=(lambda)*x], or [b*a*x=(lambda)*x].
      Here [a] and [b] are assumed to be symmetric and [b] is also
      positive definite.

      @return the vector [w] of eigenvalues in ascending order.

      @raise Failure if the function fails to converge.

      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param up default = true i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length (-> {!sygv_opt_lwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]

      @param itype specifies the problem type to be solved:
             - [`A_B] (default): a*x = (lambda)*a*x
             - [`AB]: a*b*x = (lambda)*x
             - [`BA]: b*a*x = (lambda)*x
  *)


  val sbgv :
    ?n : int ->
    ?ka : int ->
    ?kb : int ->
    ?zr : int ->
    ?zc : int ->
    ?z : mat ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat
    -> vec
  (** [sbgv ?n ?ka ?kb ?zr ?zc ?z ?up ?work ?ofsw ?w ?ar ?ac a ?br ?bc b]
      computes all the eigenvalues, and optionally, the eigenvectors of a
      real generalized symmetric-definite banded eigenproblem, of the
      form [a*x=(lambda)*b*x].  Here [a] and [b] are assumed to be
      symmetric and banded, and [b] is also positive definite.

      @return the vector [w] of eigenvalues in ascending order.

      @raise Failure if the function fails to converge.

      @param n default = available number of columns of matrix [a]
      @param z default = [None] i.e, eigenvectors are not computed
      @param up default = [true] i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length ([3 * n])
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]
  *)

  open Common
  (** {6 BLAS-1 interface} *)

  val swap :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [swap ?n ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val scal : ?n : int -> num_type -> ?ofsx : int -> ?incx : int -> vec -> unit
  (** [scal ?n alpha ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val copy :
    ?n : int ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [copy ?n ?ofsy ?incy ?y ?ofsx ?incx x] see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1 *)

  val nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
  (** [nrm2 ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
  *)

  val axpy :
    ?n : int ->
    ?alpha : num_type ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [axpy ?n ?alpha ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val iamax : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> int
  (** [iamax ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val amax :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    num_type
  (** [amax ?n ?ofsx ?incx x] @return the greater of the absolute
      values of the elements of the vector [x].
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-2 interface} *)

  val gemv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type  ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gemv ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = number of available rows in matrix [a]
      @param n default = available columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val gbmv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int ->
    int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gbmv
        ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a kl ku ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = same as [n] (i.e., [a] is a square matrix)
      @param n default = available number of columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val symv :
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?up : bool ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [symv ?n ?beta ?ofsy ?incy ?y ?up ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = dimension of symmetric matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trmv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trsv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpmv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpsv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-3 interface} *)

  val gemm :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?transa : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?transb : trans3 ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [gemm ?m ?n ?k ?beta ?cr ?cc ?c ?transa ?alpha ?ar ?ac a ?transb ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [a] (or tr [a]) and [c]
      @param n default = number of columns of [b] (or tr [b]) and [c]
      @param k default = number of columns of [a] (or tr [a]) and
                         number of rows of [b] (or tr [b])
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param transa default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param transb default = `N
      @param br default = 1
      @param bc default = 1 *)

  val symm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [symm ?m ?n ?side ?up ?beta ?cr ?cc ?c ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [c]
      @param n default = number of columns of [c]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trmm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trmm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trsm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trsm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @return matrix [b], which is overwritten.
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val syrk :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [syrk ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1 *)

  val syr2k :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [syr2k ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1
  *)


  (** {6 LAPACK interface} *)

  (** {7 Auxiliary routines} *)

  val lacpy :
    ?uplo : [ `U | `L ] ->
    ?m : int ->
    ?n : int ->
    ?br : int ->
    ?bc : int ->
    ?b : mat ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [lacpy ?uplo ?m ?n ?br ?bc ?b ?ar ?ac a] copy a (triangular)
      (sub-)matrix [a] (to an optional (sub-)matrix [b]).

      @param uplo default = whole matrix
  *)

  val lassq :
    ?n : int ->
    ?scale : float ->
    ?sumsq : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    float * float
  (** [lassq ?n ?ofsx ?incx ?scale ?sumsq] @return [(scl, ssq)], where
      [scl] is a scaling factor and [ssq] the sum of squares of vector
      [x] starting at [ofs] and using increment [incx] and initial
      [scale] and [sumsq].  See LAPACK-documentation for details!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param scale default = 0.
      @param sumsq default = 1.
  *)

  val larnv :
    ?idist : [ `Uniform0 | `Uniform1 | `Normal ] ->
    ?ofsiseed : int ->
    ?iseed : int_vec ->
    ?n : int ->
    ?ofsx : int ->
    ?x : vec ->
    unit ->
    vec
  (** [larnv ?idist ?ofsiseed ?iseed ?n ?ofsx ?x ()] @return a random
      vector with random distribution as specifified by [idist], random seed
      [iseed], vector offset [ofsx] and optional vector [x].

      @param idist default = [`Normal]
      @param ofsiseed default = [1]
      @param iseed default = integer vector of size 4 with all ones.
      @param n default = length of [x] if [x] is provided, [1] otherwise.
      @param ofsx default = [1]
      @param x default = vector of length [n] if [n] is provided.
  *)

  val lange_min_lwork : int -> norm4 -> int
  (** [lange_min_lwork m norm]
      @return the minimum length of the work array used by the [lange]-function.
      @param m the number of rows in the matrix
      @param norm type of norm that will be computed by [lange] *)

  val lange :
    ?m : int ->
    ?n : int ->
    ?norm : norm4 ->
    ?work : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [lange ?m ?n ?norm ?work ?ar ?ac a]
      @param m default = number of rows of matrix [a]
      @param n default = number of columns of matrix [a]
      @param norm default = `O
      @param work default = allocated work space for norm `I
      @param ar default = 1
      @param ac default = 1 *)

  val lauum :
    ?up : bool ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [lauum ?up ?n ?ar ?ac a]
      @param up default = [true]
      @param n default = minimum of available number of rows/columns in matrix [a]
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (computational routines)} *)

  val getrf :
    ?m : int ->
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [getrf ?m ?n ?ipiv ?ar ?ac a] computes an LU factorization of a
      general [m]-by-[n] matrix [a] using partial pivoting with row
      interchanges.  See LAPACK documentation.
      @return [ipiv], the  pivot indices.
      @raise Failure if the matrix is singular.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ipiv = vec of length [min(m, n)]
      @param ar default = 1
      @param ac default = 1 *)

  val getrs :
    ?n : int ->
    ?ipiv : int_vec ->
    ?trans : trans3 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [getrs ?n ?ipiv ?trans ?ar ?ac a ?nrhs ?br ?bc b] solves a system
      of linear equations [a] * X = [b] or [a]' * X = [b] with a general
      [n]-by-[n] matrix [a] using the LU factorization computed by
      {!getrf}.
      Note that matrix [a] will be passed to {!getrf} if [ipiv] was not
      provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = result from [getrf] applied to [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val getri_min_lwork : int -> int
  (** [getri_min_lwork n] @return the minimum length of the
      work array used by the {!getri}-function if the matrix has [n] columns. *)

  val getri_opt_lwork :
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [getri_opt_lwork ?n ?ar ?ac a] @return the optimal size of the
      work array used by the {!getri}-function.
      @param n default = number of columns of matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val getri :
    ?n : int ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [getri ?n ?ipiv ?work ?ar ?ac a] computes the inverse of a matrix
      using the LU factorization computed by {!getrf}.  Note that matrix
      [a] will be passed to {!getrf} if [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = vec of length [m] from getri
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf_min_lwork : unit -> int
  (** [sytrf_min_lwork ()] @return the minimum length of the
      work array used by the {!sytrf}-function. *)

  val sytrf_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [sytrf_opt_lwork ?n ?up ?ar ?ac a] @return the optimal size of the
      work array used by the {!sytrf}-function.
      @param n default = number of columns of matrix [a]
      @param up default = true (store upper triangle in [a])
      @param a the matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [sytrf ?n ?up ?ipiv ?work ?ar ?ac a] computes the factorization of
      the real symmetric matrix [a] using the Bunch-Kaufman diagonal
      pivoting method.
      @raise Failure if D in [a] = U*D*U' or L*D*L' is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv = vec of length n
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrs :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sytrs ?n ?up ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] solves a system of
      linear equations [a]*X = [b] with a real symmetric matrix [a]
      using the factorization [a] = U*D*U**T or [a] = L*D*L**T computed
      by {!sytrf}.  Note that matrix [a] will be passed to {!sytrf} if
      [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sytri_min_lwork : int -> int
  (** [sytri_min_lwork n] @return the minimum length of the
      work array used by the {!sytri}-function if the matrix has [n] columns. *)

  val sytri :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [sytri ?n ?up ?ipiv ?work ?ar ?ac a] computes the inverse of the
      real symmetric indefinite matrix [a] using the factorization [a] =
      U*D*U**T or [a] = L*D*L**T computed by {!sytrf}.  Note that matrix
      [a] will be passed to {!sytrf} if [ipiv] was not provided.

      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n] from {!sytrf}
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val potrf :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrf ?n ?up ?ar ?ac ?jitter a] factorizes symmetric positive
      definite matrix [a] (or the designated submatrix) using Cholesky
      factorization.

      Due to rounding errors ill-conditioned matrices may actually appear
      as if they were not positive definite, thus leading to an exception.
      One remedy for this problem is to add a small [jitter] to the
      diagonal of the matrix, which will usually allow Cholesky to complete
      successfully (though at a small bias).  For extremely ill-conditioned
      matrices it is recommended to use (symmetric) eigenvalue decomposition
      instead of this function for a numerically more stable factorization.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ar default = 1
      @param ac default = 1
      @param jitter default = nothing
  *)

  val potrs :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrs ?n ?up ?ar ?ac a ?nrhs ?br ?bc ?factorize ?jitter b] solves
      a system of linear equations [a]*X = [b], where [a] is symmetric
      positive definite matrix, using the Cholesky factorization [a] =
      U**T*U or [a] = L*L**T computed by {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val potri :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potri ?n ?up ?ar ?ac ?factorize ?jitter a] computes the inverse
      of the real symmetric positive definite matrix [a] using the
      Cholesky factorization [a] = U**T*U or [a] = L*L**T computed by
      {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param ar default = 1
      @param ac default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val trtrs :
    ?n : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trtrs ?n ?up ?trans ?diag ?ar ?ac a ?nrhs ?br ?bc b] solves a
      triangular system of the form [a] * X = [b] or [a]**T * X = [n],
      where [a] is a triangular matrix of order [n], and [b] is an
      [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val tbtrs :
    ?n : int ->
    ?kd : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [tbtrs ?n ?kd ?up ?trans ?diag ?abr ?abc ab ?nrhs ?br ?bc b]
      solves a triangular system of the form [a] * X = [b] or [a]**T * X = [b],
      where [a] is a triangular band matrix of order [n], and [b] is
      an [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [ab]
      @param kd default = number of rows in matrix [ab] - 1
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val trtri :
    ?n : int ->
    ?up : bool ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [trtri ?n ?up ?diag ?ar ?ac a] computes the inverse of a real
      upper or lower triangular matrix [a].

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [geqrf_opt_lwork ?m ?n ?ar ?ac a] @return the optimum
      length of the work-array used by the {!geqrf}-function given matrix
      [a] and optionally its logical dimensions [m] and [n].

      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_min_lwork : n : int -> int
  (** [geqrf_min_lwork ~n] @return the minimum length of the
      work-array used by the {!geqrf}-function if the matrix has [n]
      columns. *)

  val geqrf :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    vec
  (** [geqrf ?m ?n ?work ?tau ?ar ?ac a] computes a QR factorization of
      a real [m]-by-[n] matrix [a].  See LAPACK documentation.

      @return [tau], the scalar factors of the elementary reflectors.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param work default = vec of optimum length
      @param tau default = vec of required length
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (simple drivers)} *)

  val gesv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gesv ?n ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to
      a real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] matrix and X and [b] are [n]-by-[nrhs] matrices.  The
      LU decomposition with partial pivoting and row interchanges is
      used to factor [a] as [a] = P * L * U, where P is a permutation
      matrix, L is unit lower triangular, and U is upper triangular.
      The factored form of [a] is then used to solve the system of
      equations [a] * X = [b].  On exit, [b] contains the solution matrix X.

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [a]
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gbsv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    int ->
    int ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gbsv ?n ?ipiv ?abr ?abc ab kl ku ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is a band matrix of order [n] with [kl] subdiagonals and [ku]
      superdiagonals, and X and [b] are [n]-by-[nrhs] matrices.  The LU
      decomposition with partial pivoting and row interchanges is used
      to factor [a] as [a] = L * U, where L is a product of permutation and
      unit lower triangular matrices with [kl] subdiagonals, and U is
      upper triangular with [kl+ku] superdiagonals.  The factored form of
      [a] is then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [ab]
      @param ipiv default = vec of length [n]
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gtsv :
    ?n : int ->
    ?ofsdl : int ->
    vec ->
    ?ofsd : int ->
    vec ->
    ?ofsdu : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?br ?bc b] solves the
      equation [a] * X = [b] where [a] is an [n]-by-[n] tridiagonal
      matrix, by Gaussian elimination with partial pivoting.  Note that
      the equation [A]'*X = [b] may be solved by interchanging the order
      of the arguments [du] and [dl].

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsdl default = 1
      @param ofsd default = 1
      @param ofsdu default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val posv :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [posv ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to a
      real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix and X and [b] are
      [n]-by-[nrhs] matrices.  The Cholesky decomposition is used to
      factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ppsv :
    ?n : int ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ppsv ?n ?up ?ofsap ap ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The Cholesky
      decomposition is used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val pbsv :
    ?n : int ->
    ?up : bool ->
    ?kd : int ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [pbsv ?n ?up ?kd ?abr ?abc ab ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an [n]-by-[n] symmetric positive definite band matrix and X
      and [b] are [n]-by-[nrhs] matrices.  The Cholesky decomposition is
      used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular band matrix, and L is a lower
      triangular band matrix, with the same number of superdiagonals or
      subdiagonals as [a].  The factored form of [a] is then used to
      solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [ab]
      @param up default = true i.e., upper triangle of [ab] is stored
      @param kd default = available number of rows in matrix [ab] - 1
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ptsv :
    ?n : int ->
    ?ofsd : int ->
    vec ->
    ?ofse : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ptsv ?n ?ofsd d ?ofse e ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a]*X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite tridiagonal matrix, and X
      and [b] are [n]-by-[nrhs] matrices.  A is factored as [a] =
      L*D*L**T, and the factored form of [a] is then used to solve the
      system of equations.

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsd default = 1
      @param ofse default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [sysv_opt_lwork ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [sysv]-function given matrix
      [a], optionally its logical dimension [n] and given right hand side
      matrix [b] with an optional number [nrhs] of vectors.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sysv ?n ?up ?ipiv ?work ?ar ?ac a ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an N-by-N symmetric matrix and X and [b] are [n]-by-[nrhs]
      matrices.  The diagonal pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, and D is symmetric and block diagonal with
      1-by-1 and 2-by-2 diagonal blocks.  The factored form of [a] is
      then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ipiv default = vec of length [n]
      @param work default = vec of optimum length (-> [sysv_opt_lwork])
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val spsv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [spsv ?n ?up ?ipiv ?ofsap ap ?nrhs ?br ?bc b] computes the
      solution to the real system of linear equations [a] * X = [b],
      where [a] is an [n]-by-[n] symmetric matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The diagonal
      pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, D is symmetric and block diagonal with 1-by-1
      and 2-by-2 diagonal blocks.  The factored form of [a] is then used
      to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ipiv default = vec of length [n]
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


  (** {7 Least squares (simple drivers)} *)

  val gels_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gels_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gels]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gels_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [gels_opt_lwork ?m ?n ?trans ?ar ?ac a ?nrhs ?br ?bc b] @return
      the optimum length of the work-array used by the [gels]-function given
      matrix [a], optionally its logical dimensions [m] and [n] and given
      right hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gels :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gels ?m ?n ?work ?trans ?ar ?ac a ?nrhs ?br ?bc b] see
      LAPACK documentation!
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param work default = vec of optimum length (-> {!gels_opt_lwork})
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


end

(** Single precision real BLAS and LAPACK functions. *)
module S : sig

  type prec = float32_elt
  type num_type = float

  type vec = (float, float32_elt, fortran_layout) Array1.t
  (** Vectors (precision: float32). *)

  type rvec = vec

  type mat = (float, float32_elt, fortran_layout) Array2.t
  (** Matrices (precision: float32). *)

  type trans3 = [ `N | `T ]

  val prec : (float, float32_elt) Bigarray.kind
  (** Precision for this submodule {!S}.  Allows to write precision
      independent code. *)

  module Vec : sig
    (** {5 Vector operations} *)

    (** {6 Creation of vectors} *)

    val random :
      ?rnd_state : Random.State.t ->
      ?from : float -> ?range : float ->
      int
      -> vec
    (** [random ?rnd_state ?from ?range n] @return a vector
        of size [n] initialized with random elements sampled uniformly from
        [range] starting at [from].  A random state [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param from default = -1.0
        @param range default = 2.0 *)

    val sqr :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [sqr ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the square
        of [n] elements of the vector [x] using [incx] as incremental
        steps.   If [y] is given, the result will be stored in there
        using increments of [incy], otherwise a fresh vector will be
        used.  The resulting vector is returned.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1
    *)

    val sqrt :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [sqrt ?n ?ofsy ?incy ?y ?ofsx ?incx x] computes the square root
        of [n] elements of the vector [x] using [incx] as incremental
        steps.   If [y] is given, the result will be stored in there
        using increments of [incy], otherwise a fresh vector will be
        used.  The resulting vector is returned.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1
    *)

    (** {6 Creation/conversion of vectors and dimension accessor} *)

    val create : int -> vec
    (** [create n] @return a vector with [n] rows (not initialized). *)

    val make : int -> num_type -> vec
    (** [make n x] @return a vector with [n] rows initialized with value [x]. *)

    val make0 : int -> vec
    (** [make0 n x] @return a vector with [n] rows initialized with the zero
        element. *)

    val init : int -> (int -> num_type) -> vec
    (** [init n f] @return a vector containing [n] elements, where each
        element at position [i] is initialized by the result of calling
        [f i]. *)

    val of_array : num_type array -> vec
    (** [of_array ar] @return a vector initialized from array [ar]. *)

    val to_array : vec -> num_type array
    (** [to_array v] @return an array initialized from vector [v]. *)

    val of_list : num_type list -> vec
    (** [of_list l] @return a vector initialized from list [l]. *)

    val to_list : vec -> num_type list
    (** [to_list v] @return a list initialized from vector [v]. *)

    val append : vec -> vec -> vec
    (** [append v1 v2] @return the vector resulting from appending vector
        [v2] to [v1]. *)

    val concat : vec list -> vec
    (** [concat vs] @return the concatenation of vectors [vs]. *)

    val empty : vec
    (** [empty], the empty vector. *)

    val linspace : ?y : vec -> num_type -> num_type -> int -> vec
    (** [linspace ?z a b n] @return the vector [y] overwritten with [n]
        linearly spaced points between and including [a] and [b].
        @param y default = fresh vector of dim [n] *)

    val logspace : ?y : vec -> num_type -> num_type -> ?base : float -> int -> vec
    (** [logspace ?z a b base n] @return the vector [y] overwritten with [n]
        points logarithmically spaced using base [b] between and including
        [base] ** [a] and [base] ** [b].
        @param y default = fresh vector of dim [n]
        @param base default = 10.0 *)

    val dim : vec -> int
    (** [dim x] @return the dimension of vector [x]. *)


    (** {6 Iterators over vectors} *)

    val map :
      (num_type -> num_type) ->
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [map f ?n ?ofsx ?incx x] @return a new vector resulting from the
        application of [f] to each element of [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val iter :
      (num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iter ?n ?ofsx ?incx f x] applies function [f] in turn to all elements
        of vector [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val iteri :
      (int -> num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iteri ?n ?ofsx ?incx f x] same as [iter] but additionally passes
        the index of the element as first argument and the element itself
        as second argument. *)

    val fold :
      ('a -> num_type -> 'a) ->
      'a ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> 'a
    (** [fold f a ?n ?ofsx ?incx x] is
        [f (... (f (f a x.{ofsx}) x.{ofsx + incx}) ...) x.{ofsx + (n-1)*incx}]
        if [incx > 0] and the same in the reverse order of appearance of the
        [x] values if [incx < 0].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)


    (** {6 Operations on one vector} *)

    val rev : vec -> vec
    (** [rev x] reverses vector [x] (non-destructive). *)

    val max : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [max ?n ?ofsx ?incx x] computes the greater of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps. NaNs
        are ignored. If only NaNs are encountered, the negative [infinity]
        value will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val min : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [min ?n ?ofsx ?incx x] computes the smaller of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps.
        NaNs are ignored. If only NaNs are encountered, the [infinity] value
        will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [sum ?n ?ofsx ?incx x] computes the sum of the [n] elements in
        vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val prod : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [prod ?n ?ofsx ?incx x] computes the product of the [n] elements
        in vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sqr_nrm2 :
      ?stable : bool -> ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
    (** [sqr_nrm2 ?stable ?n ?c ?ofsx ?incx x] computes the square of
        the 2-norm (Euclidean norm) of vector [x] separated by [incx]
        incremental steps.  If [stable] is true, this is equivalent to
        squaring the result of calling the BLAS-function [nrm2], which
        avoids over- and underflow if possible.  If [stable] is false
        (default), [dot] will be called instead for greatly improved
        performance.

        @param stable default = [false]
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
    *)

    val ssqr :
      ?n : int ->
      ?c : num_type ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> num_type
    (** [ssqr ?n ?c ?ofsx ?incx x] computes the sum of squared differences
        of the [n] elements in vector [x] from constant [c], separated
        by [incx] incremental steps.  Please do not confuse with
        {!sqr_nrm2}!  The current function behaves differently with
        complex numbers when zero is passed in for [c].  It computes
        the square for each entry then, whereas {!sqr_nrm2} uses the
        conjugate transpose in the product.  The latter will therefore
        always return a real number.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param c default = zero
        @param ofsx default = 1
        @param incx default = 1
    *)


    (** {6 Operations on two vectors} *)

    val neg :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [neg ?n ?ofsy ?incy ?y ?ofsx ?incx x] negates [n] elements of the
        vector [x] using [incx] as incremental steps.   If [y] is given,
        the result will be stored in there using increments of [incy],
        otherwise a fresh vector will be used.  The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val add :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] adds [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val sub :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] subtracts [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val mul :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] multiplies
        [n] elements of vectors [x] and [y] elementwise, using [incx]
        and [incy] as incremental steps respectively. If [z] is given, the
        result will be stored in there using increments of [incz], otherwise
        a fresh vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val div :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] divides [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val ssqr_diff :
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> num_type
    (** [ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y] returns the sum of
        squared differences of [n] elements of vectors [x] and [y], using
        [incx] and [incy] as incremental steps respectively.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

  end

  module Mat : sig
    (** {5 Matrix operations} *)

    (** {6 Creation of matrices} *)

    val hilbert : int -> mat
    (** [hilbert n] @return an [n]x[n] Hilbert matrix. *)

    val hankel : int -> mat
    (** [hankel n] @return an [n]x[n] Hankel matrix. *)

    val pascal : int -> mat
    (** [pascal n] @return an [n]x[n] Pascal matrix. *)

    val rosser : unit -> mat
    (** [rosser n] @return 8x8 Rosser matrix. *)

    val toeplitz : vec -> mat
    (** [toeplitz v] @return the Toeplitz matrix associated with [v].
        The constant diagonals are read from left to right from [v].
        @raise Invalid_argument if the length of [v] is not an odd number. *)

    val vandermonde : vec -> mat
    (** [vandermonde v] @return the Vandermonde matrix associated with [v]. *)

    val wilkinson : int -> mat
    (** [wilkinson n] @return the [n]x[n] Wilkinson matrix.
        @raise Invalid_argument if [n] is not an odd number >= 3. *)

    val random :
      ?rnd_state : Random.State.t ->
      ?from : float -> ?range : float ->
      int -> int
      -> mat
    (** [random ?rnd_state ?from ?range m n] @return an [m]x[n] matrix
        initialized with random elements sampled uniformly from [range]
        starting at [from].  A random state [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param from default = -1.0
        @param range default = 2.0 *)

    open Common
    (** {6 Creation of matrices and accessors} *)

    val create : int -> int -> mat
    (** [create m n] @return a matrix containing [m] rows and [n] columns. *)

    val make : int -> int -> num_type -> mat
    (** [make m n x] @return a matrix containing [m] rows and [n] columns
        initialized with value [x]. *)

    val make0 : int -> int -> mat
    (** [make0 m n x] @return a matrix containing [m] rows and [n] columns
        initialized with the zero element. *)

    val of_array : num_type array array -> mat
    (** [of_array ar] @return a matrix initialized from the array of arrays
        [ar].  It is assumed that the OCaml matrix is in row major order
        (standard). *)

    val to_array : mat -> num_type array array
    (** [to_array mat] @return an array of arrays initialized from matrix
        [mat]. *)

    val of_col_vecs : vec array -> mat
    (** [of_col_vecs ar] @return a matrix whose columns are initialized from
        the array of vectors [ar].  The vectors must be of same length. *)

    val to_col_vecs : mat -> vec array
    (** [to_col_vecs mat] @return an array of column vectors initialized
        from matrix [mat]. *)

    val as_vec : mat -> vec
    (** [as_vec mat] @return a vector containing all elements of the
        matrix in column-major order.  The data is shared. *)

    val init_rows : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed row-wise. *)

    val init_cols : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed column-wise. *)

    val create_mvec : int -> mat
    (** [create_mvec m] @return a matrix with one column containing [m] rows. *)

    val make_mvec : int -> num_type -> mat
    (** [make_mvec m x] @return a matrix with one column containing [m] rows
        initialized with value [x]. *)

    val mvec_of_array : num_type array -> mat
    (** [mvec_of_array ar] @return a matrix with one column
        initialized with values from array [ar]. *)

    val mvec_to_array : mat -> num_type array
    (** [mvec_to_array mat] @return an array initialized with values from
        the first (not necessarily only) column vector of matrix [mat]. *)

    val from_col_vec : vec -> mat
    (** [from_col_vec v] @return a matrix with one column representing vector [v].
        The data is shared. *)

    val from_row_vec : vec -> mat
    (** [from_row_vec v] @return a matrix with one row representing vector [v].
        The data is shared. *)

    val empty : mat
    (** [empty], the empty matrix. *)

    val identity : int -> mat
    (** [identity n] @return the [n]x[n] identity matrix. *)

    val of_diag : vec -> mat
    (** [of_diag v] @return the diagonal matrix with diagonals elements from [v]. *)

    val dim1 : mat -> int
    (** [dim1 m] @return the first dimension of matrix [m] (number of rows). *)

    val dim2 : mat -> int
    (** [dim2 m] @return the second dimension of matrix [m] (number of columns). *)

    val col : mat -> int -> vec
    (** [col m n] @return the [n]th column of matrix [m] as a vector.
        The data is shared. *)

    val copy_row : ?vec : vec -> mat -> int -> vec
    (** [copy_row ?vec mat int] @return a copy of the [n]th row of matrix [m]
        in vector [vec].

        @param vec default = fresh vector of length [dim2 mat]
    *)


    (** {6 Matrix transformations} *)

    val transpose_copy :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?br : int -> ?bc : int -> mat ->
      unit
    (** [transpose_copy ?m ?n ?ar ?ac a ?br ?bc b] copy the transpose
        of (sub-)matrix [a] into (sub-)matrix [b].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
        @param br default = [1]
        @param bc default = [1]
    *)


    val transpose : ?m : int -> ?n : int -> ?ar : int -> ?ac : int -> mat -> mat
    (** [transpose ?m ?n ?ar ?ac aa] @return the transpose of (sub-)matrix [a].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val detri : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> unit
    (** [detri ?up ?n ?ar ?ac a] takes a triangular (sub-)matrix [a], i.e. one
        where only the upper (iff [up] is true) or lower triangle is defined,
        and makes it a symmetric matrix by mirroring the defined triangle
        along the diagonal.

        @param up default = [true]
        @param n default = [Mat.dim1 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val packed : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> vec
    (** [packed ?up ?n ?ar ?ac a] @return (sub-)matrix [a] in packed
        storage format.

        @param up default = [true]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val unpacked : ?up : bool -> ?n : int -> vec -> mat
    (** [unpacked ?up x] @return an upper or lower (depending on [up])
        triangular matrix from packed representation [vec].  The other
        triangle of the matrix will be filled with zeros.

        @param up default = [true]
        @param n default = [Vec.dim x]
    *)


    (** {6 Arithmetic and other matrix operations} *)

    val copy_diag : mat -> vec
    (** [copy_diag m] @return the diagonal of matrix [m] as a vector.
        If [m] is not a square matrix, the longest possible sequence
        of diagonal elements will be returned. *)

    val trace : mat -> num_type
    (** [trace m] @return the trace of matrix [m].  If [m] is not a
        square matrix, the sum of the longest possible sequence of
        diagonal elements will be returned. *)

    val scal :
      ?m : int -> ?n : int -> num_type -> ?ar : int -> ?ac : int -> mat -> unit
    (** [scal ?m ?n alpha ?ar ?ac a] BLAS [scal] function for (sub-)matrices. *)

    val scal_cols :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?ofs : int -> vec ->
      unit
    (** [scal_cols ?m ?n ?ar ?ac a ?ofs alphas] column-wise [scal]
        function for matrices. *)

    val scal_rows :
      ?m : int -> ?n : int ->
      ?ofs : int -> vec ->
      ?ar : int -> ?ac : int -> mat ->
      unit
    (** [scal_rows ?m ?n ?ofs alphas ?ar ?ac a] row-wise [scal]
        function for matrices. *)

    val axpy :
      ?m : int ->
      ?n : int ->
      ?alpha : num_type ->
      ?xr : int ->
      ?xc : int ->
      x : mat ->
      ?yr : int ->
      ?yc : int ->
      mat
      -> unit
    (** [axpy ?m ?n ?alpha ?xr ?xc ~x ?yr ?yc y] BLAS [axpy] function for
        matrices. *)

    val gemm_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?transa : trans3 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      vec
    (** [gemm_diag ?n ?k ?beta ?ofsy ?y ?transa ?transb ?alpha ?ar ?ac a ?br ?bc b]
        computes the diagonal of the product of the (sub-)matrices [a]
        and [b] (taking into account potential transposing), multiplying
        it with [alpha] and adding [beta] times [y], storing the result in
        [y] starting at the specified offset.  [n] elements of the diagonal
        will be computed, and [k] elements of the matrices will be part of
        the dot product associated with each diagonal element.

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param transa default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?trans : trans2 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      vec
    (** [syrk_diag ?n ?k ?beta ?ofsy ?y ?trans ?alpha ?ar ?ac a]
        computes the diagonal of the symmetric rank-k product of the
        (sub-)matrix [a], multiplying it with [alpha] and adding [beta]
        times [y], storing the result in [y] starting at the specified
        offset.  [n] elements of the diagonal will be computed, and [k]
        elements of the matrix will be part of the dot product associated
        with each diagonal element.

        @param n default = number of rows of [a] (or tr[a])
        @param k default = number of columns of [a] (or tr[a])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param trans default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val gemm_trace :
      ?n : int ->
      ?k : int ->
      ?transa : trans3 ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [gemm_trace ?n ?k ?transa ?ar ?ac a ?transb ?br ?bc b] computes
        the trace of the product of the (sub-)matrices [a] and [b]
        (taking into account potential transposing).  [n] is the number
        of rows (columns) to consider in [a], and [k] the number of
        columns (rows) in [b].

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param transa default = [`N]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_trace :
      ?n : int ->
      ?k : int ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      num_type
    (** [syrk_trace ?n ?k ?ar ?ac a] computes the trace of either [a' * a]
        or [a * a'], whichever is more efficient (results are identical),
        of the (sub-)matrix [a] multiplied by its own transpose.  [n]
        is the number of rows to consider in [a], and [k] the number
        of columns to consider.

        @param n default = number of rows of [a]
        @param k default = number of columns of [a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val symm2_trace :
      ?n : int ->
      ?upa : bool ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?upb : bool ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [symm2_trace ?n ?upa ?ar ?ac a ?upb ?br ?bc b] computes the
        trace of the product of the symmetric (sub-)matrices [a] and
        [b].  [n] is the number of rows and columns to consider in [a]
        and [b].

        @param n default = dimensions of [a] and [b]
        @param upa default = true (upper triangular portion of [a] is accessed)
        @param ar default = [1]
        @param ac default = [1]
        @param upb default = true (upper triangular portion of [b] is accessed)
        @param br default = [1]
        @param bc default = [1]
    *)


    (** {6 Iterators over matrices} *)

    val map :
      (num_type -> num_type) ->
      ?m : int ->
      ?n : int ->
      ?br : int ->
      ?bc : int ->
      ?b : mat ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> mat
    (** [map f ?m ?n ?br ?bc ?b ?ar ?ac a]
        @return matrix with [f] applied to each element of [a].
        @param m default = number of rows of [a]
        @param n default = number of columns of [a]
        @param b default = fresh matrix of size m by n *)

    val fold_cols : ('a -> vec -> 'a) -> ?n : int -> ?ac : int -> 'a -> mat -> 'a
    (** [fold_cols f ?n ?ac acc a]
        @return accumulator resulting from folding over each column vector.
        @param ac default = 1
        @param n default = number of columns of [a] *)

  end

  val pp_num : Format.formatter -> float -> unit
  (** [pp_num ppf el] is equivalent to [fprintf ppf "%G" el]. *)

  val pp_vec : (float, 'a) Io.pp_vec
  (** Pretty-printer for column vectors. *)

  val pp_mat : (float, 'a) Io.pp_mat
  (** Pretty-printer for matrices. *)


  open Common
  (** {6 BLAS-1 interface} *)

  val dot :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> float
  (** [dot ?n ?ofsy ?incy y ?ofsx ?incx x] see BLAS documentation!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param ofsx default = 1
      @param incx default = 1
  *)

  val asum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
  (** [asum ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-2 interface} *)

  val sbmv :
    ?n : int ->
    ?k : int ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?up : bool ->
    ?alpha : float ->
    ?beta : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec
    -> vec
  (** [sbmv ?n ?k ?ofsy ?incy ?y ?ar ?ac a ?up ?alpha ?beta ?ofsx ?incx x] see
      BLAS documentation!

      @return vector [y], which is overwritten.

      @param n default = number of available columns to the right of [ac].
      @param k default = number of available rows in matrix [a] - 1
      @param ofsy default = 1
      @param incy default = 1
      @param ar default = 1
      @param ac default = 1
      @param y default = uninitialized vector of minimal length (see BLAS)
      @param up default = true i.e., upper band of [a] is supplied
      @param alpha default = 1.0
      @param beta default = 0.0
      @param ofsx default = 1
      @param incx default = 1
  *)

  val ger :
    ?m : int ->
    ?n : int ->
    ?alpha : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> mat
  (** [ger ?m ?n ?alpha ?ofsx ?incx x ?ofsy ?incy y n ?ar ?ac a] see
      BLAS documentation!

      @return vector [a], which is overwritten

      @param m default = number of rows of [a]
      @param n default = number of columns of [a]
      @param alpha default = 1.0
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  val syr :
    ?n : int ->
    ?alpha : float ->
    ?up : bool ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> mat
  (** [syr ?n ?alpha ?up ?ofsx ?incx x ?ar ?ac a] see BLAS documentation!

      @return matrix [a], which is overwritten

      @param n default = number of rows of [a]
      @param alpha default = 1.0
      @param up default = true i.e., upper triangle of [a] is supplied
      @param ofsx default = 1
      @param incx default = 1
      @param ar default = 1
      @param ac default = 1
  *)

  (** {6 LAPACK interface} *)

  (** {7 Auxiliary routines} *)

  val lansy_min_lwork : int -> norm4 -> int
  (** [lansy_min_lwork m norm]
      @return the minimum length of the work array used by the [lansy]-function.
      @param norm type of norm that will be computed by [lansy]
      @param n the number of columns (and rows) in the matrix *)

  val lansy :
    ?n : int ->
    ?up : bool ->
    ?norm : norm4 ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
  (** [lansy ?norm ?up ?n ?ar ?ac ?work a] see LAPACK documentation!
      @param norm default = `O
      @param up default = true (reference upper triangular part of [a])
      @param n default = number of columns of matrix [a]
      @param work default = allocated work space for norm `I *)

  val lamch :  [ `E | `S | `B | `P | `N | `R | `M | `U | `L | `O ] -> float
  (** [lamch cmach] see LAPACK documentation! *)


  (** {7 Linear equations (computational routines)} *)

  val orgqr_min_lwork : n : int -> int
  (** [orgqr_min_lwork ~n] @return the minimum length of the
      work-array used by the [orgqr]-function if the matrix has [n]
      columns. *)

  val orgqr_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [orgqr_opt_lwork ?m ?n ?k ~tau ?ar ?ac a] @return the optimum
      length of the work-array used by the [orgqr_opt_lwork]-function
      given matrix [a], optionally its logical dimensions [m] and
      [n], and the number of reflectors [k].

      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param k default = available number of elements in vector [tau]
  *)

  val orgqr :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?work : vec ->
    tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [orgqr ?m ?n ?k ?work ~tau ?ar ?ac a] see LAPACK documentation!

      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param k default = available number of elements in vector [tau]
  *)


  val ormqr :
    ?side : side ->
    ?trans : trans2 ->
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?work : vec ->
    tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?cr : int ->
    ?cc : int ->
    mat ->
    unit
  (** [ormqr ?side ?trans ?m ?n ?k ?work ~tau ?ar ?ac a ?cr ?cc c]
      see LAPACK documentation!

      @param side default = [`L]
      @param trans default = [`N]
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param k default = available number of elements in vector [tau]
  *)


  val gecon_min_lwork : int -> int
  (** [gecon_min_lwork n] @return the minimum length of the work array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to
               the [gecon]-function *)

  val gecon_min_liwork : int -> int
  (** [gecon_min_liwork n] @return the minimum length of the iwork array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to [gecon]-function *)

  val gecon :
    ?n : int ->
    ?norm : norm2 ->
    ?anorm : float ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> float
  (** [gecon ?n ?norm ?anorm ?work ?rwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of matrix [a]
      @param n default = available number of columns of matrix [a]
      @param norm default = 1-norm
      @param anorm default = norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace
      @param iwork default = automatically allocated workspace
      @param ar default = 1
      @param ac default = 1 *)

  val sycon_min_lwork : int -> int
  (** [sycon_min_lwork n] @return the minimum length of the work array
      used by the [sycon]-function.
      @param n the logical dimensions of the matrix given to
               the [sycon]-function *)

  val sycon_min_liwork : int -> int
  (** [sycon_min_liwork n] @return the minimum length of the iwork array
      used by the [sycon]-function.
      @param n the logical dimensions of the matrix given to [sycon]-function *)

  val sycon :
      ?n : int ->
      ?up : bool ->
      ?ipiv : int_vec ->
      ?anorm : float ->
      ?work : vec ->
      ?iwork : int_vec ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> float
  (** [sycon ?n ?up ?ipiv ?anorm ?work ?iwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number
              of symmetric matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of the factorization of [a] is stored
      @param ipiv default = vec of length [n]
      @param anorm default = 1-norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace
      @param iwork default = automatically allocated workspace *)

  val pocon_min_lwork : int -> int
  (** [pocon_min_lwork n] @return the minimum length of the work array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to
               the [pocon]-function *)

  val pocon_min_liwork : int -> int
  (** [pocon_min_liwork n] @return the minimum length of the iwork array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to [pocon]-function *)

  val pocon :
      ?n : int ->
      ?up : bool ->
      ?anorm : float ->
      ?work : vec ->
      ?iwork : int_vec ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> float
  (** [pocon ?n ?up ?anorm ?work ?iwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of
              symmetric positive definite matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of Cholesky factorization
                          of [a] is stored
      @param work default = automatically allocated workspace
      @param iwork default = automatically allocated workspace
      @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)

  (** {7 Least squares (expert drivers)} *)

  val gelsy_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gelsy_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gelsy]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gelsy_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsy_opt_lwork ?m ?n ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [gelsy]-function given matrix
      [a], optionally its logical dimensions [m] and [n] and given right
      hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param nrhs default = available number of columns in matrix [b] *)

  val gelsy :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?rcond : float ->
    ?jpvt : int_vec ->
    ?work : vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsy ?m ?n ?ar ?ac a ?rcond ?jpvt ?ofswork ?work ?nrhs b] see LAPACK
      documentation!  @return the effective rank of [a].
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param rcond default = (-1) => machine precision
      @param jpvt default = vec of length [n]
      @param work default = vec of optimum length (-> [gelsy_opt_lwork])
      @param nrhs default = available number of columns in matrix [b] *)

  val gelsd_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gelsd_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gelsd]-function if the logical dimensions
      of the matrix are [m] and [n] and if there are [nrhs] right hand
      side vectors. *)

  val gelsd_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsd_opt_lwork ?m ?n ?ar ?ac a ?nrhs b] @return the optimum length of
      the work-array used by the [gelsd]-function given matrix [a],
      optionally its logical dimensions [m] and [n] and given right hand
      side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param nrhs default = available number of columns in matrix [b] *)

  val gelsd_min_iwork : int -> int -> int
  (** [gelsd_min_iwork m n] @return the minimum (= optimum) length
      of the iwork-array used by the [gelsd]-function if the logical
      dimensions of the matrix are [m] and [n]. *)

  val gelsd :
    ?m : int ->
    ?n : int ->
    ?rcond : float ->
    ?ofss : int ->
    ?s : vec ->
    ?work : vec ->
    ?iwork : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelsd ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs b]
      see LAPACK documentation!
      @return the effective rank of [a].
      @raise Failure if the function fails to converge.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param rcond default = (-1) => machine precision
      @param ofss default = 1 or ignored if [s] is not given
      @param s default = vec of length [min rows cols]
      @param work default = vec of optimum length (-> [gelsd_opt_lwork])
      @param iwork default = vec of optimum (= minimum) length
      @param nrhs default = available number of columns in matrix [b] *)

  val gelss_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gelss_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gelss]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gelss_opt_lwork :
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?m : int ->
    ?n : int ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelss_opt_lwork ?ar ?ac a ?m ?n ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [gelss]-function given matrix
      [a], optionally its logical dimensions [m] and [n] and given right
      hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param nrhs default = available number of columns in matrix [b] *)

  val gelss :
    ?m : int ->
    ?n : int ->
    ?rcond : float ->
    ?ofss : int ->
    ?s : vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [gelss ?m ?n ?rcond ?ofss ?s ?ofswork ?work ?ar ?ac a ?nrhs ?br ?bc b]
      see LAPACK documentation!
      @return the effective rank of [a].
      @raise Failure if the function fails to converge.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param rcond default = (-1) => machine precision
      @param ofss default = 1 or ignored if [s] is not given
      @param s default = vec of length [min m n]
      @param work default = vec of optimum length (-> [gelss_opt_lwork])
      @param nrhs default = available number of columns in matrix [b] *)


  (** {7 General SVD routines} *)

  val gesvd_min_lwork : m : int -> n : int -> int
  (** [gesvd_min_lwork ~m ~n] @return the minimum length of the work array
      used by the [gesvd]-function for matrices with [m] rows and [n]
      columns. *)

  val gesvd_opt_lwork :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?ar : int -> ?ac : int -> mat
    -> int

  val gesvd :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?work : vec ->
    ?ar : int -> ?ac : int -> mat
    -> vec * mat * mat

  val gesdd_liwork : m : int -> n : int -> int

  val gesdd_min_lwork : ?jobz : svd_job -> m : int -> n : int -> unit -> int
  (** [gesdd_min_lwork ?jobz ~m ~n] @return the minimum length of the
      work array used by the [gesdd]-function for matrices with [m] rows
      and [n] columns for SVD-job [jobz]. *)

  val gesdd_opt_lwork :
    ?m : int -> ?n : int ->
    ?jobz : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?iwork : int_vec ->
    ?ar : int -> ?ac : int -> mat
    -> int

  val gesdd :
    ?m : int -> ?n : int ->
    ?jobz : svd_job ->
    ?s : vec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ar : int -> ?ac : int -> mat
    -> vec * mat * mat


  (** {7 General eigenvalue problem (simple drivers)} *)

  val geev_min_lwork : ?vectors : bool -> int -> int
  (** [geev_min_lwork vectors n] @return the minimum length of the
      work array used by the [geev]-function. [vectors] indicates whether
      eigenvectors are supposed to be computed.
      @param n the logical dimensions of the matrix given to [geev]-function
      @param vectors default = true *)

  val geev_opt_lwork :
    ?n : int ->
    ?vlr : int -> ?vlc : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofswr : int -> ?wr : vec ->
    ?ofswi : int -> ?wi : vec ->
    ?ar : int -> ?ac : int -> mat ->
    int
   (** [geev_opt_lwork
         ?n
         ?vlr ?vlc ?vl
         ?vrr ?vrc ?vr
         ?ofswr wr
         ?ofswi wi
         ?ar ?ac a]
      See [geev]-function for details about arguments.
      @return "optimal" size of work array. *)

  val geev :
    ?n : int ->
    ?work : vec ->
    ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofswr : int -> ?wr : vec ->
    ?ofswi : int -> ?wi : vec ->
    ?ar : int -> ?ac : int -> mat ->
    mat * vec * vec * mat
  (** [geev ?work ?n
        ?vlr ?vlc ?vl
        ?vrr ?vrc ?vr
        ?ofswr wr ?ofswi wi
        ?ar ?ac a]
      @return ([lv], [wr], [wi], [rv]), where [wr] and [wv] are the real
        and imaginary components of the eigenvalues, and [lv] and [rv]
        are the left and right eigenvectors. [lv] ([rv]) is the empty
        matrix if [vl] ([vr]) is set to [None].
      @raise Failure if the function fails to converge
      @param n default = available number of columns of matrix [a]
      @param work default = automatically allocated workspace
      @param vl default = Automatically allocated left eigenvectors.
                          Pass [None] if you do not want to compute them,
                          [Some lv] if you want to provide the storage.
                          You can set [vlr], [vlc] in the last case.
      (See LAPACK GEEV docs for details about storage of complex eigenvectors)
      @param vr default = Automatically allocated right eigenvectors.
                          Pass [None] if you do not want to compute them,
                          [Some rv] if you want to provide the storage.
                          You can set [vrr], [vrc] in the last case.
      @param wr default = vector of size [n]; real components of the eigenvalues
      @param wi default = vector of size [n];
                          imaginary components of the eigenvalues
      @param a the matrix whose eigensystem is computed *)


  (** {7 Symmetric-matrix eigenvalue and singular value problems
      (simple drivers)} *)

  val syev_min_lwork : int -> int
  (** [syev_min_lwork n] @return the minimum length of the work-array
      used by the {!syev}-function if the logical dimensions of the matrix
      are [n]. *)

  val syev_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syev_opt_lwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
      length of the work-array used by the {!syev}-function given matrix
      [a], optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syev :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> vec
  (** [syev ?n ?vectors ?up ?ofswork ?work ?ofsw ?w ?ar ?ac a] computes
      all eigenvalues and, optionally, eigenvectors of the real symmetric
      matrix [a].

      @return the vector [w] of eigenvalues in ascending order.
      @raise Failure if the function fails to converge.
      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param up default = true i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length (-> {!syev_opt_lwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n] *)

  val syevd_min_lwork : vectors : bool -> int -> int
  (** [syevd_min_lwork vectors n] @return the minimum length of the
      work-array used by the {!syevd}-function if the logical dimensions of
      the matrix are [n] and given whether eigenvectors should be computed
      ([vectors]). *)

  val syevd_min_liwork : vectors : bool -> int -> int
  (** [syevd_min_liwork vectors n] @return the minimum length of the
      iwork-array used by the {!syevd}-function if the logical dimensions of
      the matrix are [n] and given whether eigenvectors should be computed
      ([vectors]). *)

  val syevd_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevd_opt_lwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
      length of the work-array used by the {!syevd}-function given matrix
      [a], optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevd_opt_liwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevd_opt_liwork ?n ?vectors ?up ?ar ?ac a] @return the optimum
      length of the iwork-array used by the {!syevd}-function given matrix
      [a], optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevd_opt_l_li_work :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int * int
  (** [syevd_opt_l_li_iwork ?n ?vectors ?up ?ar ?ac a] @return the tuple
      of optimum lengths of the work- and iwork-arrays respectively,
      used by the {!syevd}-function given matrix [a], optionally its
      logical dimension [n] and whether the eigenvectors must be computed
      ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevd :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> vec
  (** [syevd ?n ?vectors ?up ?ofswork ?work ?iwork ?ofsw ?w ?ar ?ac a]
      computes all eigenvalues and, optionally, eigenvectors of the real
      symmetric matrix [a].  If eigenvectors are desired, it uses a
      divide and conquer algorithm.

      @return the vector [w] of eigenvalues in ascending order.
      @raise Failure if the function fails to converge.
      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param up default = true i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length (-> {!syev_opt_lwork})
      @param iwork default = int_vec of optimum length (-> {!syevd_opt_liwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n] *)

  val sbev_min_lwork : int -> int
  (** [sbev_min_lwork n] @return the minimum length of the work-array
      used by the {!sbev}-function if the logical dimensions of the matrix
      are [n]. *)

  val sbev :
    ?n : int ->
    ?kd : int ->
    ?zr : int ->
    ?zc : int ->
    ?z : mat ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?abr : int ->
    ?abc : int ->
    mat
    -> vec
  (** [sbev ?n ?vectors ?zr ?zc ?z ?up ?ofswork ?work ?ofsw ?w ?abr ?abc ab]
      computes all the eigenvalues and, optionally, eigenvectors of the
      real symmetric {i band} matrix [ab].

      @raise Failure if the function fails to converge.

      @return the vector [w] of eigenvalues in ascending order.
      @raise Failure if the function fails to converge.
      @param n default = available number of columns of matrix [ab]
      @param z matrix to contain the orthonormal eigenvectors of [ab],
               the [i]-th column of [z] holding the eigenvector associated
               with [w.{i}].
               default = [None] i.e, eigenvectors are not computed
      @param kd default = number of rows in matrix [ab] - 1
      @param up default = true i.e., upper triangle of the matrix is stored
      @param work default = vec of minimal length (-> {!sbev_min_lwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]
      @param abr default = 1
      @param abc default = 1 *)


  (** {7 Symmetric-matrix eigenvalue and singular value problems (expert &
      RRR drivers)} *)

  val syevr_min_lwork : int -> int
  (** [syevr_min_lwork n] @return the minimum length of the
      work-array used by the {!syevr}-function if the logical dimensions
      of the matrix are [n]. *)

  val syevr_min_liwork : int -> int
  (** [syevr_min_liwork n] @return the minimum length of the
      iwork-array used by the {!syevr}-function if the logical dimensions
      of the matrix are [n]. *)

  val syevr_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevr_opt_lwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a] @return
      the optimum length of the work-array used by the {!syevr}-function
      given matrix [a], optionally its logical dimension [n] and whether
      the eigenvectors must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevr_opt_liwork :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int
  (** [syevr_opt_liwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a] @return
      the optimum length of the iwork-array used by the {!syevr}-function
      given matrix [a], optionally its logical dimension [n] and whether
      the eigenvectors must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevr_opt_l_li_work :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int * int
  (** [syevr_opt_l_li_iwork ?n ?vectors ?range ?up ?abstol ?ar ?ac a]
      @return the tuple of optimum lengths of the work- and iwork-arrays
      respectively, used by the {!syevr}-function given matrix [a],
      optionally its logical dimension [n] and whether the eigenvectors
      must be computed ([vectors]).
      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored *)

  val syevr :
    ?n : int ->
    ?vectors : bool ->
    ?range : [ `A | `V of float * float | `I of int * int ] ->
    ?up : bool ->
    ?abstol : float ->
    ?work : vec ->
    ?iwork : int_vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?zr : int ->
    ?zc : int ->
    ?z : mat ->
    ?isuppz : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat
    -> int * vec * mat * int_vec
  (** [syevr
        ?n ?vectors ?range ?up ?abstol ?work ?iwork
        ?ofsw ?w ?zr ?zc ?z ?isuppz ?ar ?ac a]
      [range] is either [`A] for computing all eigenpairs, [`V (vl, vu)]
      defines the lower and upper range of computed eigenvalues, [`I (il,
      iu)] defines the indexes of the computed eigenpairs, which are sorted
      in ascending order.
      @return the tuple [(m, w, z, isuppz)], where [m] is the number
              of computed eigenpairs, vector [w] contains the computed
              eigenvalues in ascending order, [z] contains the computed
              eigenvectors in same order, and [isuppz] indicates the
              nonzero elements in [z].
      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param range default = `A
      @param up default = true i.e., upper triangle of [a] is stored
      @param abstol default = result of calling [lamch `S]
      @param work default = vec of optimum length (-> {!syev_opt_lwork})
      @param iwork default = int_vec of optimum length (-> {!syevr_opt_liwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]
      @param zr default = 1
      @param zc default = 1
      @param z default = matrix with minimal required dimension
      @param isuppz default = [int_vec] with minimal required dimension
      @param ar default = 1
      @param ac default = 1 *)

  val sygv_opt_lwork :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?itype : [`A_B | `AB | `BA ] ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat
    -> int
  (** [sygv_opt_lwork ?n ?vectors ?up ?ar ?ac a ?br ?bc b] @return the
      optimum length of the work-array used by the {!sygv}-function
      for the given matrices [a] and [b], optionally their logical
      dimension [n] and whether the eigenvectors must be computed
      ([vectors]).

      @param n default = available number of columns of matrix [a]
      @param vectors default = false, i.e. eigenvectors are not computed
      @param up default = true, i.e. upper triangle of [a] is stored

      @param itype specifies the problem type to be solved:
             - [`A_B] (default): a*x = (lambda)*a*x
             - [`AB]: a*b*x = (lambda)*x
             - [`BA]: b*a*x = (lambda)*x
  *)

  val sygv :
    ?n : int ->
    ?vectors : bool ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?itype : [`A_B | `AB | `BA ] ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat
    -> vec
  (** [sygv ?n ?vectors ?up ?ofswork ?work ?ofsw ?w ?ar ?ac a]
      computes all the eigenvalues, and optionally, the eigenvectors
      of a real generalized symmetric-definite eigenproblem, of the
      form [a*x=(lambda)*b*x], [a*b*x=(lambda)*x], or [b*a*x=(lambda)*x].
      Here [a] and [b] are assumed to be symmetric and [b] is also
      positive definite.

      @return the vector [w] of eigenvalues in ascending order.

      @raise Failure if the function fails to converge.

      @param n default = available number of columns of matrix [a]
      @param vectors default = false i.e, eigenvectors are not computed
      @param up default = true i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length (-> {!sygv_opt_lwork})
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]

      @param itype specifies the problem type to be solved:
             - [`A_B] (default): a*x = (lambda)*a*x
             - [`AB]: a*b*x = (lambda)*x
             - [`BA]: b*a*x = (lambda)*x
  *)


  val sbgv :
    ?n : int ->
    ?ka : int ->
    ?kb : int ->
    ?zr : int ->
    ?zc : int ->
    ?z : mat ->
    ?up : bool ->
    ?work : vec ->
    ?ofsw : int ->
    ?w : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat
    -> vec
  (** [sbgv ?n ?ka ?kb ?zr ?zc ?z ?up ?work ?ofsw ?w ?ar ?ac a ?br ?bc b]
      computes all the eigenvalues, and optionally, the eigenvectors of a
      real generalized symmetric-definite banded eigenproblem, of the
      form [a*x=(lambda)*b*x].  Here [a] and [b] are assumed to be
      symmetric and banded, and [b] is also positive definite.

      @return the vector [w] of eigenvalues in ascending order.

      @raise Failure if the function fails to converge.

      @param n default = available number of columns of matrix [a]
      @param z default = [None] i.e, eigenvectors are not computed
      @param up default = [true] i.e., upper triangle of [a] is stored
      @param work default = vec of optimum length ([3 * n])
      @param ofsw default = 1 or ignored if [w] is not given
      @param w default = vec of length [n]
  *)

  open Common
  (** {6 BLAS-1 interface} *)

  val swap :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [swap ?n ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val scal : ?n : int -> num_type -> ?ofsx : int -> ?incx : int -> vec -> unit
  (** [scal ?n alpha ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val copy :
    ?n : int ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [copy ?n ?ofsy ?incy ?y ?ofsx ?incx x] see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1 *)

  val nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
  (** [nrm2 ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
  *)

  val axpy :
    ?n : int ->
    ?alpha : num_type ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [axpy ?n ?alpha ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val iamax : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> int
  (** [iamax ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val amax :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    num_type
  (** [amax ?n ?ofsx ?incx x] @return the greater of the absolute
      values of the elements of the vector [x].
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-2 interface} *)

  val gemv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type  ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gemv ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = number of available rows in matrix [a]
      @param n default = available columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val gbmv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int ->
    int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gbmv
        ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a kl ku ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = same as [n] (i.e., [a] is a square matrix)
      @param n default = available number of columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val symv :
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?up : bool ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [symv ?n ?beta ?ofsy ?incy ?y ?up ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = dimension of symmetric matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trmv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trsv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpmv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpsv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-3 interface} *)

  val gemm :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?transa : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?transb : trans3 ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [gemm ?m ?n ?k ?beta ?cr ?cc ?c ?transa ?alpha ?ar ?ac a ?transb ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [a] (or tr [a]) and [c]
      @param n default = number of columns of [b] (or tr [b]) and [c]
      @param k default = number of columns of [a] (or tr [a]) and
                         number of rows of [b] (or tr [b])
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param transa default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param transb default = `N
      @param br default = 1
      @param bc default = 1 *)

  val symm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [symm ?m ?n ?side ?up ?beta ?cr ?cc ?c ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [c]
      @param n default = number of columns of [c]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trmm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trmm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trsm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trsm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @return matrix [b], which is overwritten.
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val syrk :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [syrk ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1 *)

  val syr2k :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [syr2k ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1
  *)


  (** {6 LAPACK interface} *)

  (** {7 Auxiliary routines} *)

  val lacpy :
    ?uplo : [ `U | `L ] ->
    ?m : int ->
    ?n : int ->
    ?br : int ->
    ?bc : int ->
    ?b : mat ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [lacpy ?uplo ?m ?n ?br ?bc ?b ?ar ?ac a] copy a (triangular)
      (sub-)matrix [a] (to an optional (sub-)matrix [b]).

      @param uplo default = whole matrix
  *)

  val lassq :
    ?n : int ->
    ?scale : float ->
    ?sumsq : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    float * float
  (** [lassq ?n ?ofsx ?incx ?scale ?sumsq] @return [(scl, ssq)], where
      [scl] is a scaling factor and [ssq] the sum of squares of vector
      [x] starting at [ofs] and using increment [incx] and initial
      [scale] and [sumsq].  See LAPACK-documentation for details!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param scale default = 0.
      @param sumsq default = 1.
  *)

  val larnv :
    ?idist : [ `Uniform0 | `Uniform1 | `Normal ] ->
    ?ofsiseed : int ->
    ?iseed : int_vec ->
    ?n : int ->
    ?ofsx : int ->
    ?x : vec ->
    unit ->
    vec
  (** [larnv ?idist ?ofsiseed ?iseed ?n ?ofsx ?x ()] @return a random
      vector with random distribution as specifified by [idist], random seed
      [iseed], vector offset [ofsx] and optional vector [x].

      @param idist default = [`Normal]
      @param ofsiseed default = [1]
      @param iseed default = integer vector of size 4 with all ones.
      @param n default = length of [x] if [x] is provided, [1] otherwise.
      @param ofsx default = [1]
      @param x default = vector of length [n] if [n] is provided.
  *)

  val lange_min_lwork : int -> norm4 -> int
  (** [lange_min_lwork m norm]
      @return the minimum length of the work array used by the [lange]-function.
      @param m the number of rows in the matrix
      @param norm type of norm that will be computed by [lange] *)

  val lange :
    ?m : int ->
    ?n : int ->
    ?norm : norm4 ->
    ?work : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [lange ?m ?n ?norm ?work ?ar ?ac a]
      @param m default = number of rows of matrix [a]
      @param n default = number of columns of matrix [a]
      @param norm default = `O
      @param work default = allocated work space for norm `I
      @param ar default = 1
      @param ac default = 1 *)

  val lauum :
    ?up : bool ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [lauum ?up ?n ?ar ?ac a]
      @param up default = [true]
      @param n default = minimum of available number of rows/columns in matrix [a]
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (computational routines)} *)

  val getrf :
    ?m : int ->
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [getrf ?m ?n ?ipiv ?ar ?ac a] computes an LU factorization of a
      general [m]-by-[n] matrix [a] using partial pivoting with row
      interchanges.  See LAPACK documentation.
      @return [ipiv], the  pivot indices.
      @raise Failure if the matrix is singular.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ipiv = vec of length [min(m, n)]
      @param ar default = 1
      @param ac default = 1 *)

  val getrs :
    ?n : int ->
    ?ipiv : int_vec ->
    ?trans : trans3 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [getrs ?n ?ipiv ?trans ?ar ?ac a ?nrhs ?br ?bc b] solves a system
      of linear equations [a] * X = [b] or [a]' * X = [b] with a general
      [n]-by-[n] matrix [a] using the LU factorization computed by
      {!getrf}.
      Note that matrix [a] will be passed to {!getrf} if [ipiv] was not
      provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = result from [getrf] applied to [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val getri_min_lwork : int -> int
  (** [getri_min_lwork n] @return the minimum length of the
      work array used by the {!getri}-function if the matrix has [n] columns. *)

  val getri_opt_lwork :
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [getri_opt_lwork ?n ?ar ?ac a] @return the optimal size of the
      work array used by the {!getri}-function.
      @param n default = number of columns of matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val getri :
    ?n : int ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [getri ?n ?ipiv ?work ?ar ?ac a] computes the inverse of a matrix
      using the LU factorization computed by {!getrf}.  Note that matrix
      [a] will be passed to {!getrf} if [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = vec of length [m] from getri
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf_min_lwork : unit -> int
  (** [sytrf_min_lwork ()] @return the minimum length of the
      work array used by the {!sytrf}-function. *)

  val sytrf_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [sytrf_opt_lwork ?n ?up ?ar ?ac a] @return the optimal size of the
      work array used by the {!sytrf}-function.
      @param n default = number of columns of matrix [a]
      @param up default = true (store upper triangle in [a])
      @param a the matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [sytrf ?n ?up ?ipiv ?work ?ar ?ac a] computes the factorization of
      the real symmetric matrix [a] using the Bunch-Kaufman diagonal
      pivoting method.
      @raise Failure if D in [a] = U*D*U' or L*D*L' is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv = vec of length n
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrs :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sytrs ?n ?up ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] solves a system of
      linear equations [a]*X = [b] with a real symmetric matrix [a]
      using the factorization [a] = U*D*U**T or [a] = L*D*L**T computed
      by {!sytrf}.  Note that matrix [a] will be passed to {!sytrf} if
      [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sytri_min_lwork : int -> int
  (** [sytri_min_lwork n] @return the minimum length of the
      work array used by the {!sytri}-function if the matrix has [n] columns. *)

  val sytri :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [sytri ?n ?up ?ipiv ?work ?ar ?ac a] computes the inverse of the
      real symmetric indefinite matrix [a] using the factorization [a] =
      U*D*U**T or [a] = L*D*L**T computed by {!sytrf}.  Note that matrix
      [a] will be passed to {!sytrf} if [ipiv] was not provided.

      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n] from {!sytrf}
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val potrf :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrf ?n ?up ?ar ?ac ?jitter a] factorizes symmetric positive
      definite matrix [a] (or the designated submatrix) using Cholesky
      factorization.

      Due to rounding errors ill-conditioned matrices may actually appear
      as if they were not positive definite, thus leading to an exception.
      One remedy for this problem is to add a small [jitter] to the
      diagonal of the matrix, which will usually allow Cholesky to complete
      successfully (though at a small bias).  For extremely ill-conditioned
      matrices it is recommended to use (symmetric) eigenvalue decomposition
      instead of this function for a numerically more stable factorization.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ar default = 1
      @param ac default = 1
      @param jitter default = nothing
  *)

  val potrs :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrs ?n ?up ?ar ?ac a ?nrhs ?br ?bc ?factorize ?jitter b] solves
      a system of linear equations [a]*X = [b], where [a] is symmetric
      positive definite matrix, using the Cholesky factorization [a] =
      U**T*U or [a] = L*L**T computed by {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val potri :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potri ?n ?up ?ar ?ac ?factorize ?jitter a] computes the inverse
      of the real symmetric positive definite matrix [a] using the
      Cholesky factorization [a] = U**T*U or [a] = L*L**T computed by
      {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param ar default = 1
      @param ac default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val trtrs :
    ?n : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trtrs ?n ?up ?trans ?diag ?ar ?ac a ?nrhs ?br ?bc b] solves a
      triangular system of the form [a] * X = [b] or [a]**T * X = [n],
      where [a] is a triangular matrix of order [n], and [b] is an
      [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val tbtrs :
    ?n : int ->
    ?kd : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [tbtrs ?n ?kd ?up ?trans ?diag ?abr ?abc ab ?nrhs ?br ?bc b]
      solves a triangular system of the form [a] * X = [b] or [a]**T * X = [b],
      where [a] is a triangular band matrix of order [n], and [b] is
      an [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [ab]
      @param kd default = number of rows in matrix [ab] - 1
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val trtri :
    ?n : int ->
    ?up : bool ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [trtri ?n ?up ?diag ?ar ?ac a] computes the inverse of a real
      upper or lower triangular matrix [a].

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [geqrf_opt_lwork ?m ?n ?ar ?ac a] @return the optimum
      length of the work-array used by the {!geqrf}-function given matrix
      [a] and optionally its logical dimensions [m] and [n].

      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_min_lwork : n : int -> int
  (** [geqrf_min_lwork ~n] @return the minimum length of the
      work-array used by the {!geqrf}-function if the matrix has [n]
      columns. *)

  val geqrf :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    vec
  (** [geqrf ?m ?n ?work ?tau ?ar ?ac a] computes a QR factorization of
      a real [m]-by-[n] matrix [a].  See LAPACK documentation.

      @return [tau], the scalar factors of the elementary reflectors.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param work default = vec of optimum length
      @param tau default = vec of required length
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (simple drivers)} *)

  val gesv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gesv ?n ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to
      a real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] matrix and X and [b] are [n]-by-[nrhs] matrices.  The
      LU decomposition with partial pivoting and row interchanges is
      used to factor [a] as [a] = P * L * U, where P is a permutation
      matrix, L is unit lower triangular, and U is upper triangular.
      The factored form of [a] is then used to solve the system of
      equations [a] * X = [b].  On exit, [b] contains the solution matrix X.

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [a]
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gbsv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    int ->
    int ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gbsv ?n ?ipiv ?abr ?abc ab kl ku ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is a band matrix of order [n] with [kl] subdiagonals and [ku]
      superdiagonals, and X and [b] are [n]-by-[nrhs] matrices.  The LU
      decomposition with partial pivoting and row interchanges is used
      to factor [a] as [a] = L * U, where L is a product of permutation and
      unit lower triangular matrices with [kl] subdiagonals, and U is
      upper triangular with [kl+ku] superdiagonals.  The factored form of
      [a] is then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [ab]
      @param ipiv default = vec of length [n]
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gtsv :
    ?n : int ->
    ?ofsdl : int ->
    vec ->
    ?ofsd : int ->
    vec ->
    ?ofsdu : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?br ?bc b] solves the
      equation [a] * X = [b] where [a] is an [n]-by-[n] tridiagonal
      matrix, by Gaussian elimination with partial pivoting.  Note that
      the equation [A]'*X = [b] may be solved by interchanging the order
      of the arguments [du] and [dl].

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsdl default = 1
      @param ofsd default = 1
      @param ofsdu default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val posv :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [posv ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to a
      real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix and X and [b] are
      [n]-by-[nrhs] matrices.  The Cholesky decomposition is used to
      factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ppsv :
    ?n : int ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ppsv ?n ?up ?ofsap ap ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The Cholesky
      decomposition is used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val pbsv :
    ?n : int ->
    ?up : bool ->
    ?kd : int ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [pbsv ?n ?up ?kd ?abr ?abc ab ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an [n]-by-[n] symmetric positive definite band matrix and X
      and [b] are [n]-by-[nrhs] matrices.  The Cholesky decomposition is
      used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular band matrix, and L is a lower
      triangular band matrix, with the same number of superdiagonals or
      subdiagonals as [a].  The factored form of [a] is then used to
      solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [ab]
      @param up default = true i.e., upper triangle of [ab] is stored
      @param kd default = available number of rows in matrix [ab] - 1
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ptsv :
    ?n : int ->
    ?ofsd : int ->
    vec ->
    ?ofse : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ptsv ?n ?ofsd d ?ofse e ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a]*X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite tridiagonal matrix, and X
      and [b] are [n]-by-[nrhs] matrices.  A is factored as [a] =
      L*D*L**T, and the factored form of [a] is then used to solve the
      system of equations.

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsd default = 1
      @param ofse default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [sysv_opt_lwork ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [sysv]-function given matrix
      [a], optionally its logical dimension [n] and given right hand side
      matrix [b] with an optional number [nrhs] of vectors.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sysv ?n ?up ?ipiv ?work ?ar ?ac a ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an N-by-N symmetric matrix and X and [b] are [n]-by-[nrhs]
      matrices.  The diagonal pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, and D is symmetric and block diagonal with
      1-by-1 and 2-by-2 diagonal blocks.  The factored form of [a] is
      then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ipiv default = vec of length [n]
      @param work default = vec of optimum length (-> [sysv_opt_lwork])
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val spsv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [spsv ?n ?up ?ipiv ?ofsap ap ?nrhs ?br ?bc b] computes the
      solution to the real system of linear equations [a] * X = [b],
      where [a] is an [n]-by-[n] symmetric matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The diagonal
      pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, D is symmetric and block diagonal with 1-by-1
      and 2-by-2 diagonal blocks.  The factored form of [a] is then used
      to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ipiv default = vec of length [n]
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


  (** {7 Least squares (simple drivers)} *)

  val gels_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gels_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gels]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gels_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [gels_opt_lwork ?m ?n ?trans ?ar ?ac a ?nrhs ?br ?bc b] @return
      the optimum length of the work-array used by the [gels]-function given
      matrix [a], optionally its logical dimensions [m] and [n] and given
      right hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gels :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gels ?m ?n ?work ?trans ?ar ?ac a ?nrhs ?br ?bc b] see
      LAPACK documentation!
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param work default = vec of optimum length (-> {!gels_opt_lwork})
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


end

(** Double precision complex BLAS and LAPACK functions. *)
module Z : sig

  type prec = complex64_elt
  type num_type = Complex.t

  type vec = (Complex.t, complex64_elt, fortran_layout) Array1.t
  (** Complex vectors (precision: complex64). *)

  type rvec = (float, float64_elt, fortran_layout) Array1.t
  (** Vectors of reals (precision: float64). *)

  type mat = (Complex.t, complex64_elt, fortran_layout) Array2.t
  (** Complex matrices (precision: complex64). *)

  type trans3 = [ `C | `N | `T ]
  val prec : (Complex.t, complex64_elt) Bigarray.kind
  (** Precision for this submodule {!Z}.  Allows to write precision
      independent code. *)

  module Vec : sig
    (** {5 Vector operations} *)

    (** {6 Creation of vectors} *)

    val random :
      ?rnd_state : Random.State.t ->
      ?re_from : float -> ?re_range : float ->
      ?im_from : float -> ?im_range : float ->
      int
      -> vec
    (** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range n]
        @return a vector of size [n] initialized with random elements sampled
        uniformly from [re_range] and [im_range] starting at [re_from] and
        [im_from] for real and imaginary numbers respectively.  A random state
        [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param re_from default = -1.0
        @param re_range default = 2.0
        @param im_from default = -1.0
        @param im_range default = 2.0 *)

    (** {6 Creation/conversion of vectors and dimension accessor} *)

    val create : int -> vec
    (** [create n] @return a vector with [n] rows (not initialized). *)

    val make : int -> num_type -> vec
    (** [make n x] @return a vector with [n] rows initialized with value [x]. *)

    val make0 : int -> vec
    (** [make0 n x] @return a vector with [n] rows initialized with the zero
        element. *)

    val init : int -> (int -> num_type) -> vec
    (** [init n f] @return a vector containing [n] elements, where each
        element at position [i] is initialized by the result of calling
        [f i]. *)

    val of_array : num_type array -> vec
    (** [of_array ar] @return a vector initialized from array [ar]. *)

    val to_array : vec -> num_type array
    (** [to_array v] @return an array initialized from vector [v]. *)

    val of_list : num_type list -> vec
    (** [of_list l] @return a vector initialized from list [l]. *)

    val to_list : vec -> num_type list
    (** [to_list v] @return a list initialized from vector [v]. *)

    val append : vec -> vec -> vec
    (** [append v1 v2] @return the vector resulting from appending vector
        [v2] to [v1]. *)

    val concat : vec list -> vec
    (** [concat vs] @return the concatenation of vectors [vs]. *)

    val empty : vec
    (** [empty], the empty vector. *)

    val linspace : ?y : vec -> num_type -> num_type -> int -> vec
    (** [linspace ?z a b n] @return the vector [y] overwritten with [n]
        linearly spaced points between and including [a] and [b].
        @param y default = fresh vector of dim [n] *)

    val logspace : ?y : vec -> num_type -> num_type -> ?base : float -> int -> vec
    (** [logspace ?z a b base n] @return the vector [y] overwritten with [n]
        points logarithmically spaced using base [b] between and including
        [base] ** [a] and [base] ** [b].
        @param y default = fresh vector of dim [n]
        @param base default = 10.0 *)

    val dim : vec -> int
    (** [dim x] @return the dimension of vector [x]. *)


    (** {6 Iterators over vectors} *)

    val map :
      (num_type -> num_type) ->
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [map f ?n ?ofsx ?incx x] @return a new vector resulting from the
        application of [f] to each element of [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val iter :
      (num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iter ?n ?ofsx ?incx f x] applies function [f] in turn to all elements
        of vector [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val iteri :
      (int -> num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iteri ?n ?ofsx ?incx f x] same as [iter] but additionally passes
        the index of the element as first argument and the element itself
        as second argument. *)

    val fold :
      ('a -> num_type -> 'a) ->
      'a ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> 'a
    (** [fold f a ?n ?ofsx ?incx x] is
        [f (... (f (f a x.{ofsx}) x.{ofsx + incx}) ...) x.{ofsx + (n-1)*incx}]
        if [incx > 0] and the same in the reverse order of appearance of the
        [x] values if [incx < 0].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)


    (** {6 Operations on one vector} *)

    val rev : vec -> vec
    (** [rev x] reverses vector [x] (non-destructive). *)

    val max : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [max ?n ?ofsx ?incx x] computes the greater of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps. NaNs
        are ignored. If only NaNs are encountered, the negative [infinity]
        value will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val min : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [min ?n ?ofsx ?incx x] computes the smaller of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps.
        NaNs are ignored. If only NaNs are encountered, the [infinity] value
        will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [sum ?n ?ofsx ?incx x] computes the sum of the [n] elements in
        vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val prod : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [prod ?n ?ofsx ?incx x] computes the product of the [n] elements
        in vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sqr_nrm2 :
      ?stable : bool -> ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
    (** [sqr_nrm2 ?stable ?n ?c ?ofsx ?incx x] computes the square of
        the 2-norm (Euclidean norm) of vector [x] separated by [incx]
        incremental steps.  If [stable] is true, this is equivalent to
        squaring the result of calling the BLAS-function [nrm2], which
        avoids over- and underflow if possible.  If [stable] is false
        (default), [dot] will be called instead for greatly improved
        performance.

        @param stable default = [false]
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
    *)

    val ssqr :
      ?n : int ->
      ?c : num_type ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> num_type
    (** [ssqr ?n ?c ?ofsx ?incx x] computes the sum of squared differences
        of the [n] elements in vector [x] from constant [c], separated
        by [incx] incremental steps.  Please do not confuse with
        {!sqr_nrm2}!  The current function behaves differently with
        complex numbers when zero is passed in for [c].  It computes
        the square for each entry then, whereas {!sqr_nrm2} uses the
        conjugate transpose in the product.  The latter will therefore
        always return a real number.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param c default = zero
        @param ofsx default = 1
        @param incx default = 1
    *)


    (** {6 Operations on two vectors} *)

    val neg :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [neg ?n ?ofsy ?incy ?y ?ofsx ?incx x] negates [n] elements of the
        vector [x] using [incx] as incremental steps.   If [y] is given,
        the result will be stored in there using increments of [incy],
        otherwise a fresh vector will be used.  The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val add :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] adds [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val sub :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] subtracts [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val mul :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] multiplies
        [n] elements of vectors [x] and [y] elementwise, using [incx]
        and [incy] as incremental steps respectively. If [z] is given, the
        result will be stored in there using increments of [incz], otherwise
        a fresh vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val div :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] divides [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val ssqr_diff :
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> num_type
    (** [ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y] returns the sum of
        squared differences of [n] elements of vectors [x] and [y], using
        [incx] and [incy] as incremental steps respectively.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

  end

  module Mat : sig
    (** {5 Matrix operations} *)

    (** {6 Creation of matrices} *)

    val random :
      ?rnd_state : Random.State.t ->
      ?re_from : float -> ?re_range : float ->
      ?im_from : float -> ?im_range : float ->
      int -> int
      -> mat
    (** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range m n]
        @return an [m]x[n] matrix initialized with random elements sampled
        uniformly from [re_range] and [im_range] starting at [re_from] and
        [im_from] for real and imaginary numbers respectively.  A random state
        [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param re_from default = -1.0
        @param re_range default = 2.0
        @param im_from default = -1.0
        @param im_range default = 2.0 *)

    open Common
    (** {6 Creation of matrices and accessors} *)

    val create : int -> int -> mat
    (** [create m n] @return a matrix containing [m] rows and [n] columns. *)

    val make : int -> int -> num_type -> mat
    (** [make m n x] @return a matrix containing [m] rows and [n] columns
        initialized with value [x]. *)

    val make0 : int -> int -> mat
    (** [make0 m n x] @return a matrix containing [m] rows and [n] columns
        initialized with the zero element. *)

    val of_array : num_type array array -> mat
    (** [of_array ar] @return a matrix initialized from the array of arrays
        [ar].  It is assumed that the OCaml matrix is in row major order
        (standard). *)

    val to_array : mat -> num_type array array
    (** [to_array mat] @return an array of arrays initialized from matrix
        [mat]. *)

    val of_col_vecs : vec array -> mat
    (** [of_col_vecs ar] @return a matrix whose columns are initialized from
        the array of vectors [ar].  The vectors must be of same length. *)

    val to_col_vecs : mat -> vec array
    (** [to_col_vecs mat] @return an array of column vectors initialized
        from matrix [mat]. *)

    val as_vec : mat -> vec
    (** [as_vec mat] @return a vector containing all elements of the
        matrix in column-major order.  The data is shared. *)

    val init_rows : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed row-wise. *)

    val init_cols : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed column-wise. *)

    val create_mvec : int -> mat
    (** [create_mvec m] @return a matrix with one column containing [m] rows. *)

    val make_mvec : int -> num_type -> mat
    (** [make_mvec m x] @return a matrix with one column containing [m] rows
        initialized with value [x]. *)

    val mvec_of_array : num_type array -> mat
    (** [mvec_of_array ar] @return a matrix with one column
        initialized with values from array [ar]. *)

    val mvec_to_array : mat -> num_type array
    (** [mvec_to_array mat] @return an array initialized with values from
        the first (not necessarily only) column vector of matrix [mat]. *)

    val from_col_vec : vec -> mat
    (** [from_col_vec v] @return a matrix with one column representing vector [v].
        The data is shared. *)

    val from_row_vec : vec -> mat
    (** [from_row_vec v] @return a matrix with one row representing vector [v].
        The data is shared. *)

    val empty : mat
    (** [empty], the empty matrix. *)

    val identity : int -> mat
    (** [identity n] @return the [n]x[n] identity matrix. *)

    val of_diag : vec -> mat
    (** [of_diag v] @return the diagonal matrix with diagonals elements from [v]. *)

    val dim1 : mat -> int
    (** [dim1 m] @return the first dimension of matrix [m] (number of rows). *)

    val dim2 : mat -> int
    (** [dim2 m] @return the second dimension of matrix [m] (number of columns). *)

    val col : mat -> int -> vec
    (** [col m n] @return the [n]th column of matrix [m] as a vector.
        The data is shared. *)

    val copy_row : ?vec : vec -> mat -> int -> vec
    (** [copy_row ?vec mat int] @return a copy of the [n]th row of matrix [m]
        in vector [vec].

        @param vec default = fresh vector of length [dim2 mat]
    *)


    (** {6 Matrix transformations} *)

    val transpose_copy :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?br : int -> ?bc : int -> mat ->
      unit
    (** [transpose_copy ?m ?n ?ar ?ac a ?br ?bc b] copy the transpose
        of (sub-)matrix [a] into (sub-)matrix [b].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
        @param br default = [1]
        @param bc default = [1]
    *)


    val transpose : ?m : int -> ?n : int -> ?ar : int -> ?ac : int -> mat -> mat
    (** [transpose ?m ?n ?ar ?ac aa] @return the transpose of (sub-)matrix [a].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val detri : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> unit
    (** [detri ?up ?n ?ar ?ac a] takes a triangular (sub-)matrix [a], i.e. one
        where only the upper (iff [up] is true) or lower triangle is defined,
        and makes it a symmetric matrix by mirroring the defined triangle
        along the diagonal.

        @param up default = [true]
        @param n default = [Mat.dim1 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val packed : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> vec
    (** [packed ?up ?n ?ar ?ac a] @return (sub-)matrix [a] in packed
        storage format.

        @param up default = [true]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val unpacked : ?up : bool -> ?n : int -> vec -> mat
    (** [unpacked ?up x] @return an upper or lower (depending on [up])
        triangular matrix from packed representation [vec].  The other
        triangle of the matrix will be filled with zeros.

        @param up default = [true]
        @param n default = [Vec.dim x]
    *)


    (** {6 Arithmetic and other matrix operations} *)

    val copy_diag : mat -> vec
    (** [copy_diag m] @return the diagonal of matrix [m] as a vector.
        If [m] is not a square matrix, the longest possible sequence
        of diagonal elements will be returned. *)

    val trace : mat -> num_type
    (** [trace m] @return the trace of matrix [m].  If [m] is not a
        square matrix, the sum of the longest possible sequence of
        diagonal elements will be returned. *)

    val scal :
      ?m : int -> ?n : int -> num_type -> ?ar : int -> ?ac : int -> mat -> unit
    (** [scal ?m ?n alpha ?ar ?ac a] BLAS [scal] function for (sub-)matrices. *)

    val scal_cols :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?ofs : int -> vec ->
      unit
    (** [scal_cols ?m ?n ?ar ?ac a ?ofs alphas] column-wise [scal]
        function for matrices. *)

    val scal_rows :
      ?m : int -> ?n : int ->
      ?ofs : int -> vec ->
      ?ar : int -> ?ac : int -> mat ->
      unit
    (** [scal_rows ?m ?n ?ofs alphas ?ar ?ac a] row-wise [scal]
        function for matrices. *)

    val axpy :
      ?m : int ->
      ?n : int ->
      ?alpha : num_type ->
      ?xr : int ->
      ?xc : int ->
      x : mat ->
      ?yr : int ->
      ?yc : int ->
      mat
      -> unit
    (** [axpy ?m ?n ?alpha ?xr ?xc ~x ?yr ?yc y] BLAS [axpy] function for
        matrices. *)

    val gemm_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?transa : trans3 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      vec
    (** [gemm_diag ?n ?k ?beta ?ofsy ?y ?transa ?transb ?alpha ?ar ?ac a ?br ?bc b]
        computes the diagonal of the product of the (sub-)matrices [a]
        and [b] (taking into account potential transposing), multiplying
        it with [alpha] and adding [beta] times [y], storing the result in
        [y] starting at the specified offset.  [n] elements of the diagonal
        will be computed, and [k] elements of the matrices will be part of
        the dot product associated with each diagonal element.

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param transa default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?trans : trans2 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      vec
    (** [syrk_diag ?n ?k ?beta ?ofsy ?y ?trans ?alpha ?ar ?ac a]
        computes the diagonal of the symmetric rank-k product of the
        (sub-)matrix [a], multiplying it with [alpha] and adding [beta]
        times [y], storing the result in [y] starting at the specified
        offset.  [n] elements of the diagonal will be computed, and [k]
        elements of the matrix will be part of the dot product associated
        with each diagonal element.

        @param n default = number of rows of [a] (or tr[a])
        @param k default = number of columns of [a] (or tr[a])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param trans default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val gemm_trace :
      ?n : int ->
      ?k : int ->
      ?transa : trans3 ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [gemm_trace ?n ?k ?transa ?ar ?ac a ?transb ?br ?bc b] computes
        the trace of the product of the (sub-)matrices [a] and [b]
        (taking into account potential transposing).  [n] is the number
        of rows (columns) to consider in [a], and [k] the number of
        columns (rows) in [b].

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param transa default = [`N]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_trace :
      ?n : int ->
      ?k : int ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      num_type
    (** [syrk_trace ?n ?k ?ar ?ac a] computes the trace of either [a' * a]
        or [a * a'], whichever is more efficient (results are identical),
        of the (sub-)matrix [a] multiplied by its own transpose.  [n]
        is the number of rows to consider in [a], and [k] the number
        of columns to consider.

        @param n default = number of rows of [a]
        @param k default = number of columns of [a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val symm2_trace :
      ?n : int ->
      ?upa : bool ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?upb : bool ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [symm2_trace ?n ?upa ?ar ?ac a ?upb ?br ?bc b] computes the
        trace of the product of the symmetric (sub-)matrices [a] and
        [b].  [n] is the number of rows and columns to consider in [a]
        and [b].

        @param n default = dimensions of [a] and [b]
        @param upa default = true (upper triangular portion of [a] is accessed)
        @param ar default = [1]
        @param ac default = [1]
        @param upb default = true (upper triangular portion of [b] is accessed)
        @param br default = [1]
        @param bc default = [1]
    *)


    (** {6 Iterators over matrices} *)

    val map :
      (num_type -> num_type) ->
      ?m : int ->
      ?n : int ->
      ?br : int ->
      ?bc : int ->
      ?b : mat ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> mat
    (** [map f ?m ?n ?br ?bc ?b ?ar ?ac a]
        @return matrix with [f] applied to each element of [a].
        @param m default = number of rows of [a]
        @param n default = number of columns of [a]
        @param b default = fresh matrix of size m by n *)

    val fold_cols : ('a -> vec -> 'a) -> ?n : int -> ?ac : int -> 'a -> mat -> 'a
    (** [fold_cols f ?n ?ac acc a]
        @return accumulator resulting from folding over each column vector.
        @param ac default = 1
        @param n default = number of columns of [a] *)

  end

  val pp_num : Format.formatter -> Complex.t -> unit
  (** [pp_num ppf el] is equivalent to [fprintf ppf "(%G, %Gi)"
      el.re el.im]. *)

  val pp_vec : (Complex.t, 'a) Io.pp_vec
  (** Pretty-printer for column vectors. *)

  val pp_mat : (Complex.t, 'a) Io.pp_mat
  (** Pretty-printer for matrices. *)


  open Common
  (** {6 BLAS-1 interface} *)

  val dotu :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> num_type
  (** [dotu ?n ?ofsy ?incy y ?ofsx ?incx x] see BLAS documentation!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param ofsx default = 1
      @param incx default = 1
  *)

  val dotc :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> num_type
  (** [dotc ?n ?ofsy ?incy y ?ofsx ?incx x] see BLAS documentation!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param ofsx default = 1
      @param incx default = 1
  *)


  (** {6 LAPACK interface} *)

  val lansy_min_lwork : int -> norm4 -> int
  (** [lansy_min_lwork m norm]
      @return the minimum length of the work array used by the [lansy]-function.
      @param norm type of norm that will be computed by [lansy]
      @param n the number of columns (and rows) in the matrix *)

  val lansy :
    ?n : int ->
    ?up : bool ->
    ?norm : norm4 ->
    ?work : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [lansy ?n ?up ?norm ?work ?ar ?ac a] see LAPACK documentation!
      @param norm default = `O
      @param up default = true (reference upper triangular part of [a])
      @param n default = number of columns of matrix [a]
      @param work default = allocated work space for norm `I *)

  val gecon_min_lwork : int -> int
  (** [gecon_min_lwork n] @return the minimum length of the work array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to
               the [gecon]-function *)

  val gecon_min_lrwork : int -> int
  (** [gecon_min_lrwork n] @return the minimum length of the rwork array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to [gecon]-function *)

  val gecon :
    ?n : int ->
    ?norm : norm2 ->
    ?anorm : float ->
    ?work : vec ->
    ?rwork : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [gecon ?n ?norm ?anorm ?work ?rwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of matrix [a]
      @param n default = available number of columns of matrix [a]
      @param norm default = 1-norm
      @param anorm default = norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace
      @param rwork default = automatically allocated workspace
      @param ar default = 1
      @param ac default = 1 *)

  val sycon_min_lwork : int -> int
  (** [sycon_min_lwork n] @return the minimum length of the work array
      used by the [sycon]-function.
      @param n the logical dimensions of the matrix given to
               the [sycon]-function *)

  val sycon :
      ?n : int ->
      ?up : bool ->
      ?ipiv : int_vec ->
      ?anorm : float ->
      ?work : vec ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      float
  (** [sycon ?n ?up ?ipiv ?anorm ?work ?ar ?ac a]
      @return estimate of the reciprocal of the
              condition number of symmetric matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of the factorization of [a] is stored
      @param ipiv default = vec of length [n]
      @param anorm default = 1-norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace *)

  val pocon_min_lwork : int -> int
  (** [pocon_min_lwork n] @return the minimum length of the work array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to
               the [pocon]-function *)

  val pocon_min_lrwork : int -> int
  (** [pocon_min_lrwork n] @return the minimum length of the rwork array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to [pocon]-function *)

  val pocon :
      ?n : int ->
      ?up : bool ->
      ?anorm : float ->
      ?work : vec ->
      ?rwork : rvec ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      float
  (** [pocon ?n ?up ?anorm ?work ?rwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of
              complex Hermitian positive definite matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of Cholesky factorization
                          of [a] is stored
      @param work default = automatically allocated workspace
      @param rwork default = automatically allocated workspace
      @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)


  (** {7 General SVD routines} *)

  val gesvd_min_lwork : m : int -> n : int -> int
  (** [gesvd_min_lwork ~m ~n] @return the minimum length of the work array
      used by the [gesvd]-function for matrices with [m] rows and [n]
      columns. *)

  val gesvd_lrwork : m : int -> n : int -> int
  (** [gesvd_lrwork m n] @return the (minimum) length of the rwork array
      used by the [gesvd]-function. *)

  val gesvd_opt_lwork :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : rvec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?ar : int -> ?ac : int -> mat ->
    int

  val gesvd :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : rvec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?work : vec ->
    ?rwork : rvec ->
    ?ar : int -> ?ac : int -> mat ->
    rvec * mat * mat


  (** {7 General eigenvalue problem (simple drivers)} *)

  val geev_min_lwork : int -> int
  (** [geev_min_lwork n] @return the minimum length of the work array
      used by the [geev]-function.
      @param n the logical dimensions of the matrix given to [geev]-function *)

  val geev_min_lrwork : int -> int
  (** [geev_min_lrwork n] @return the minimum length of the rwork array
      used by the [geev]-function.
      @param n the logical dimensions of the matrix given to [geev]-function *)

  val geev_opt_lwork :
    ?n : int ->
    ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofsw : int -> ?w : vec ->
    ?ar : int -> ?ac : int -> mat ->
    int
   (** [geev ?work ?rwork ?n ?vlr ?vlc ?vl
         ?vrr ?vrc ?vr ?ofsw w ?ar ?ac a]
      See [geev]-function for details about arguments.
      @return "optimal" work size *)

  val geev :
    ?n : int ->
    ?work : vec ->
    ?rwork : vec ->
    ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofsw : int -> ?w : vec ->
    ?ar : int -> ?ac : int -> mat ->
    mat * vec * mat
  (** [geev ?work ?rwork ?n
        ?vlr ?vlc ?vl
        ?vrr ?vrc ?vr
        ?ofsw w
        ?ar ?ac a]
      @return [(lv, w, rv)], where [lv] and [rv] correspond to the left and
        right eigenvectors respectively, [w] to the eigenvalues. [lv] ([rv])
        is the empty matrix if [vl] ([vr]) is set to [None].
      @raise Failure if the function fails to converge
      @param n default = available number of columns of matrix [a]
      @param work default = automatically allocated workspace
      @param rwork default = automatically allocated workspace
      @param vl default = Automatically allocated left eigenvectors.
                          Pass [None] if you do not want to compute them,
                          [Some lv] if you want to provide the storage.
                          You can set [vlr], [vlc] in the last case.
      (See LAPACK GEEV docs for details about storage of complex eigenvectors)
      @param vr default = Automatically allocated right eigenvectors.
                           Pass [None] if you do not want to compute them,
                           [Some rv] if you want to provide the storage.
                           You can set [vrr], [vrc] in the last case.
      @param w default = automatically allocate eigenvalues
      @param a the matrix whose eigensystem is computed *)

  open Common
  (** {6 BLAS-1 interface} *)

  val swap :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [swap ?n ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val scal : ?n : int -> num_type -> ?ofsx : int -> ?incx : int -> vec -> unit
  (** [scal ?n alpha ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val copy :
    ?n : int ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [copy ?n ?ofsy ?incy ?y ?ofsx ?incx x] see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1 *)

  val nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
  (** [nrm2 ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
  *)

  val axpy :
    ?n : int ->
    ?alpha : num_type ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [axpy ?n ?alpha ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val iamax : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> int
  (** [iamax ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val amax :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    num_type
  (** [amax ?n ?ofsx ?incx x] @return the greater of the absolute
      values of the elements of the vector [x].
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-2 interface} *)

  val gemv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type  ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gemv ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = number of available rows in matrix [a]
      @param n default = available columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val gbmv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int ->
    int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gbmv
        ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a kl ku ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = same as [n] (i.e., [a] is a square matrix)
      @param n default = available number of columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val symv :
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?up : bool ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [symv ?n ?beta ?ofsy ?incy ?y ?up ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = dimension of symmetric matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trmv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trsv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpmv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpsv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-3 interface} *)

  val gemm :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?transa : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?transb : trans3 ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [gemm ?m ?n ?k ?beta ?cr ?cc ?c ?transa ?alpha ?ar ?ac a ?transb ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [a] (or tr [a]) and [c]
      @param n default = number of columns of [b] (or tr [b]) and [c]
      @param k default = number of columns of [a] (or tr [a]) and
                         number of rows of [b] (or tr [b])
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param transa default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param transb default = `N
      @param br default = 1
      @param bc default = 1 *)

  val symm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [symm ?m ?n ?side ?up ?beta ?cr ?cc ?c ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [c]
      @param n default = number of columns of [c]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trmm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trmm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trsm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trsm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @return matrix [b], which is overwritten.
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val syrk :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [syrk ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1 *)

  val syr2k :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [syr2k ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1
  *)


  (** {6 LAPACK interface} *)

  (** {7 Auxiliary routines} *)

  val lacpy :
    ?uplo : [ `U | `L ] ->
    ?m : int ->
    ?n : int ->
    ?br : int ->
    ?bc : int ->
    ?b : mat ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [lacpy ?uplo ?m ?n ?br ?bc ?b ?ar ?ac a] copy a (triangular)
      (sub-)matrix [a] (to an optional (sub-)matrix [b]).

      @param uplo default = whole matrix
  *)

  val lassq :
    ?n : int ->
    ?scale : float ->
    ?sumsq : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    float * float
  (** [lassq ?n ?ofsx ?incx ?scale ?sumsq] @return [(scl, ssq)], where
      [scl] is a scaling factor and [ssq] the sum of squares of vector
      [x] starting at [ofs] and using increment [incx] and initial
      [scale] and [sumsq].  See LAPACK-documentation for details!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param scale default = 0.
      @param sumsq default = 1.
  *)

  val larnv :
    ?idist : [ `Uniform0 | `Uniform1 | `Normal ] ->
    ?ofsiseed : int ->
    ?iseed : int_vec ->
    ?n : int ->
    ?ofsx : int ->
    ?x : vec ->
    unit ->
    vec
  (** [larnv ?idist ?ofsiseed ?iseed ?n ?ofsx ?x ()] @return a random
      vector with random distribution as specifified by [idist], random seed
      [iseed], vector offset [ofsx] and optional vector [x].

      @param idist default = [`Normal]
      @param ofsiseed default = [1]
      @param iseed default = integer vector of size 4 with all ones.
      @param n default = length of [x] if [x] is provided, [1] otherwise.
      @param ofsx default = [1]
      @param x default = vector of length [n] if [n] is provided.
  *)

  val lange_min_lwork : int -> norm4 -> int
  (** [lange_min_lwork m norm]
      @return the minimum length of the work array used by the [lange]-function.
      @param m the number of rows in the matrix
      @param norm type of norm that will be computed by [lange] *)

  val lange :
    ?m : int ->
    ?n : int ->
    ?norm : norm4 ->
    ?work : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [lange ?m ?n ?norm ?work ?ar ?ac a]
      @param m default = number of rows of matrix [a]
      @param n default = number of columns of matrix [a]
      @param norm default = `O
      @param work default = allocated work space for norm `I
      @param ar default = 1
      @param ac default = 1 *)

  val lauum :
    ?up : bool ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [lauum ?up ?n ?ar ?ac a]
      @param up default = [true]
      @param n default = minimum of available number of rows/columns in matrix [a]
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (computational routines)} *)

  val getrf :
    ?m : int ->
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [getrf ?m ?n ?ipiv ?ar ?ac a] computes an LU factorization of a
      general [m]-by-[n] matrix [a] using partial pivoting with row
      interchanges.  See LAPACK documentation.
      @return [ipiv], the  pivot indices.
      @raise Failure if the matrix is singular.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ipiv = vec of length [min(m, n)]
      @param ar default = 1
      @param ac default = 1 *)

  val getrs :
    ?n : int ->
    ?ipiv : int_vec ->
    ?trans : trans3 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [getrs ?n ?ipiv ?trans ?ar ?ac a ?nrhs ?br ?bc b] solves a system
      of linear equations [a] * X = [b] or [a]' * X = [b] with a general
      [n]-by-[n] matrix [a] using the LU factorization computed by
      {!getrf}.
      Note that matrix [a] will be passed to {!getrf} if [ipiv] was not
      provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = result from [getrf] applied to [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val getri_min_lwork : int -> int
  (** [getri_min_lwork n] @return the minimum length of the
      work array used by the {!getri}-function if the matrix has [n] columns. *)

  val getri_opt_lwork :
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [getri_opt_lwork ?n ?ar ?ac a] @return the optimal size of the
      work array used by the {!getri}-function.
      @param n default = number of columns of matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val getri :
    ?n : int ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [getri ?n ?ipiv ?work ?ar ?ac a] computes the inverse of a matrix
      using the LU factorization computed by {!getrf}.  Note that matrix
      [a] will be passed to {!getrf} if [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = vec of length [m] from getri
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf_min_lwork : unit -> int
  (** [sytrf_min_lwork ()] @return the minimum length of the
      work array used by the {!sytrf}-function. *)

  val sytrf_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [sytrf_opt_lwork ?n ?up ?ar ?ac a] @return the optimal size of the
      work array used by the {!sytrf}-function.
      @param n default = number of columns of matrix [a]
      @param up default = true (store upper triangle in [a])
      @param a the matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [sytrf ?n ?up ?ipiv ?work ?ar ?ac a] computes the factorization of
      the real symmetric matrix [a] using the Bunch-Kaufman diagonal
      pivoting method.
      @raise Failure if D in [a] = U*D*U' or L*D*L' is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv = vec of length n
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrs :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sytrs ?n ?up ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] solves a system of
      linear equations [a]*X = [b] with a real symmetric matrix [a]
      using the factorization [a] = U*D*U**T or [a] = L*D*L**T computed
      by {!sytrf}.  Note that matrix [a] will be passed to {!sytrf} if
      [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sytri_min_lwork : int -> int
  (** [sytri_min_lwork n] @return the minimum length of the
      work array used by the {!sytri}-function if the matrix has [n] columns. *)

  val sytri :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [sytri ?n ?up ?ipiv ?work ?ar ?ac a] computes the inverse of the
      real symmetric indefinite matrix [a] using the factorization [a] =
      U*D*U**T or [a] = L*D*L**T computed by {!sytrf}.  Note that matrix
      [a] will be passed to {!sytrf} if [ipiv] was not provided.

      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n] from {!sytrf}
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val potrf :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrf ?n ?up ?ar ?ac ?jitter a] factorizes symmetric positive
      definite matrix [a] (or the designated submatrix) using Cholesky
      factorization.

      Due to rounding errors ill-conditioned matrices may actually appear
      as if they were not positive definite, thus leading to an exception.
      One remedy for this problem is to add a small [jitter] to the
      diagonal of the matrix, which will usually allow Cholesky to complete
      successfully (though at a small bias).  For extremely ill-conditioned
      matrices it is recommended to use (symmetric) eigenvalue decomposition
      instead of this function for a numerically more stable factorization.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ar default = 1
      @param ac default = 1
      @param jitter default = nothing
  *)

  val potrs :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrs ?n ?up ?ar ?ac a ?nrhs ?br ?bc ?factorize ?jitter b] solves
      a system of linear equations [a]*X = [b], where [a] is symmetric
      positive definite matrix, using the Cholesky factorization [a] =
      U**T*U or [a] = L*L**T computed by {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val potri :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potri ?n ?up ?ar ?ac ?factorize ?jitter a] computes the inverse
      of the real symmetric positive definite matrix [a] using the
      Cholesky factorization [a] = U**T*U or [a] = L*L**T computed by
      {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param ar default = 1
      @param ac default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val trtrs :
    ?n : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trtrs ?n ?up ?trans ?diag ?ar ?ac a ?nrhs ?br ?bc b] solves a
      triangular system of the form [a] * X = [b] or [a]**T * X = [n],
      where [a] is a triangular matrix of order [n], and [b] is an
      [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val tbtrs :
    ?n : int ->
    ?kd : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [tbtrs ?n ?kd ?up ?trans ?diag ?abr ?abc ab ?nrhs ?br ?bc b]
      solves a triangular system of the form [a] * X = [b] or [a]**T * X = [b],
      where [a] is a triangular band matrix of order [n], and [b] is
      an [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [ab]
      @param kd default = number of rows in matrix [ab] - 1
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val trtri :
    ?n : int ->
    ?up : bool ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [trtri ?n ?up ?diag ?ar ?ac a] computes the inverse of a real
      upper or lower triangular matrix [a].

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [geqrf_opt_lwork ?m ?n ?ar ?ac a] @return the optimum
      length of the work-array used by the {!geqrf}-function given matrix
      [a] and optionally its logical dimensions [m] and [n].

      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_min_lwork : n : int -> int
  (** [geqrf_min_lwork ~n] @return the minimum length of the
      work-array used by the {!geqrf}-function if the matrix has [n]
      columns. *)

  val geqrf :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    vec
  (** [geqrf ?m ?n ?work ?tau ?ar ?ac a] computes a QR factorization of
      a real [m]-by-[n] matrix [a].  See LAPACK documentation.

      @return [tau], the scalar factors of the elementary reflectors.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param work default = vec of optimum length
      @param tau default = vec of required length
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (simple drivers)} *)

  val gesv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gesv ?n ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to
      a real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] matrix and X and [b] are [n]-by-[nrhs] matrices.  The
      LU decomposition with partial pivoting and row interchanges is
      used to factor [a] as [a] = P * L * U, where P is a permutation
      matrix, L is unit lower triangular, and U is upper triangular.
      The factored form of [a] is then used to solve the system of
      equations [a] * X = [b].  On exit, [b] contains the solution matrix X.

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [a]
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gbsv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    int ->
    int ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gbsv ?n ?ipiv ?abr ?abc ab kl ku ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is a band matrix of order [n] with [kl] subdiagonals and [ku]
      superdiagonals, and X and [b] are [n]-by-[nrhs] matrices.  The LU
      decomposition with partial pivoting and row interchanges is used
      to factor [a] as [a] = L * U, where L is a product of permutation and
      unit lower triangular matrices with [kl] subdiagonals, and U is
      upper triangular with [kl+ku] superdiagonals.  The factored form of
      [a] is then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [ab]
      @param ipiv default = vec of length [n]
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gtsv :
    ?n : int ->
    ?ofsdl : int ->
    vec ->
    ?ofsd : int ->
    vec ->
    ?ofsdu : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?br ?bc b] solves the
      equation [a] * X = [b] where [a] is an [n]-by-[n] tridiagonal
      matrix, by Gaussian elimination with partial pivoting.  Note that
      the equation [A]'*X = [b] may be solved by interchanging the order
      of the arguments [du] and [dl].

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsdl default = 1
      @param ofsd default = 1
      @param ofsdu default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val posv :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [posv ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to a
      real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix and X and [b] are
      [n]-by-[nrhs] matrices.  The Cholesky decomposition is used to
      factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ppsv :
    ?n : int ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ppsv ?n ?up ?ofsap ap ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The Cholesky
      decomposition is used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val pbsv :
    ?n : int ->
    ?up : bool ->
    ?kd : int ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [pbsv ?n ?up ?kd ?abr ?abc ab ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an [n]-by-[n] symmetric positive definite band matrix and X
      and [b] are [n]-by-[nrhs] matrices.  The Cholesky decomposition is
      used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular band matrix, and L is a lower
      triangular band matrix, with the same number of superdiagonals or
      subdiagonals as [a].  The factored form of [a] is then used to
      solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [ab]
      @param up default = true i.e., upper triangle of [ab] is stored
      @param kd default = available number of rows in matrix [ab] - 1
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ptsv :
    ?n : int ->
    ?ofsd : int ->
    vec ->
    ?ofse : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ptsv ?n ?ofsd d ?ofse e ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a]*X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite tridiagonal matrix, and X
      and [b] are [n]-by-[nrhs] matrices.  A is factored as [a] =
      L*D*L**T, and the factored form of [a] is then used to solve the
      system of equations.

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsd default = 1
      @param ofse default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [sysv_opt_lwork ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [sysv]-function given matrix
      [a], optionally its logical dimension [n] and given right hand side
      matrix [b] with an optional number [nrhs] of vectors.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sysv ?n ?up ?ipiv ?work ?ar ?ac a ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an N-by-N symmetric matrix and X and [b] are [n]-by-[nrhs]
      matrices.  The diagonal pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, and D is symmetric and block diagonal with
      1-by-1 and 2-by-2 diagonal blocks.  The factored form of [a] is
      then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ipiv default = vec of length [n]
      @param work default = vec of optimum length (-> [sysv_opt_lwork])
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val spsv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [spsv ?n ?up ?ipiv ?ofsap ap ?nrhs ?br ?bc b] computes the
      solution to the real system of linear equations [a] * X = [b],
      where [a] is an [n]-by-[n] symmetric matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The diagonal
      pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, D is symmetric and block diagonal with 1-by-1
      and 2-by-2 diagonal blocks.  The factored form of [a] is then used
      to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ipiv default = vec of length [n]
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


  (** {7 Least squares (simple drivers)} *)

  val gels_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gels_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gels]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gels_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [gels_opt_lwork ?m ?n ?trans ?ar ?ac a ?nrhs ?br ?bc b] @return
      the optimum length of the work-array used by the [gels]-function given
      matrix [a], optionally its logical dimensions [m] and [n] and given
      right hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gels :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gels ?m ?n ?work ?trans ?ar ?ac a ?nrhs ?br ?bc b] see
      LAPACK documentation!
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param work default = vec of optimum length (-> {!gels_opt_lwork})
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


end

(** Single precision complex BLAS and LAPACK functions. *)
module C : sig

  type prec = complex32_elt
  type num_type = Complex.t

  type vec = (Complex.t, complex32_elt, fortran_layout) Array1.t
  (** Complex vectors (precision: complex32). *)

  type rvec = (float, float32_elt, fortran_layout) Array1.t
  (** Vectors of reals (precision: float32). *)

  type mat = (Complex.t, complex32_elt, fortran_layout) Array2.t
  (** Complex matrices (precision: complex32). *)

  type trans3 = [ `C | `N | `T ]
  val prec : (Complex.t, complex32_elt) Bigarray.kind
  (** Precision for this submodule {!C}.  Allows to write precision
      independent code. *)

  module Vec : sig
    (** {5 Vector operations} *)

    (** {6 Creation of vectors} *)

    val random :
      ?rnd_state : Random.State.t ->
      ?re_from : float -> ?re_range : float ->
      ?im_from : float -> ?im_range : float ->
      int
      -> vec
    (** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range n]
        @return a vector of size [n] initialized with random elements sampled
        uniformly from [re_range] and [im_range] starting at [re_from] and
        [im_from] for real and imaginary numbers respectively.  A random state
        [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param re_from default = -1.0
        @param re_range default = 2.0
        @param im_from default = -1.0
        @param im_range default = 2.0 *)

    (** {6 Creation/conversion of vectors and dimension accessor} *)

    val create : int -> vec
    (** [create n] @return a vector with [n] rows (not initialized). *)

    val make : int -> num_type -> vec
    (** [make n x] @return a vector with [n] rows initialized with value [x]. *)

    val make0 : int -> vec
    (** [make0 n x] @return a vector with [n] rows initialized with the zero
        element. *)

    val init : int -> (int -> num_type) -> vec
    (** [init n f] @return a vector containing [n] elements, where each
        element at position [i] is initialized by the result of calling
        [f i]. *)

    val of_array : num_type array -> vec
    (** [of_array ar] @return a vector initialized from array [ar]. *)

    val to_array : vec -> num_type array
    (** [to_array v] @return an array initialized from vector [v]. *)

    val of_list : num_type list -> vec
    (** [of_list l] @return a vector initialized from list [l]. *)

    val to_list : vec -> num_type list
    (** [to_list v] @return a list initialized from vector [v]. *)

    val append : vec -> vec -> vec
    (** [append v1 v2] @return the vector resulting from appending vector
        [v2] to [v1]. *)

    val concat : vec list -> vec
    (** [concat vs] @return the concatenation of vectors [vs]. *)

    val empty : vec
    (** [empty], the empty vector. *)

    val linspace : ?y : vec -> num_type -> num_type -> int -> vec
    (** [linspace ?z a b n] @return the vector [y] overwritten with [n]
        linearly spaced points between and including [a] and [b].
        @param y default = fresh vector of dim [n] *)

    val logspace : ?y : vec -> num_type -> num_type -> ?base : float -> int -> vec
    (** [logspace ?z a b base n] @return the vector [y] overwritten with [n]
        points logarithmically spaced using base [b] between and including
        [base] ** [a] and [base] ** [b].
        @param y default = fresh vector of dim [n]
        @param base default = 10.0 *)

    val dim : vec -> int
    (** [dim x] @return the dimension of vector [x]. *)


    (** {6 Iterators over vectors} *)

    val map :
      (num_type -> num_type) ->
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [map f ?n ?ofsx ?incx x] @return a new vector resulting from the
        application of [f] to each element of [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val iter :
      (num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iter ?n ?ofsx ?incx f x] applies function [f] in turn to all elements
        of vector [x].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val iteri :
      (int -> num_type -> unit) ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> unit
    (** [iteri ?n ?ofsx ?incx f x] same as [iter] but additionally passes
        the index of the element as first argument and the element itself
        as second argument. *)

    val fold :
      ('a -> num_type -> 'a) ->
      'a ->
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> 'a
    (** [fold f a ?n ?ofsx ?incx x] is
        [f (... (f (f a x.{ofsx}) x.{ofsx + incx}) ...) x.{ofsx + (n-1)*incx}]
        if [incx > 0] and the same in the reverse order of appearance of the
        [x] values if [incx < 0].
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)


    (** {6 Operations on one vector} *)

    val rev : vec -> vec
    (** [rev x] reverses vector [x] (non-destructive). *)

    val max : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [max ?n ?ofsx ?incx x] computes the greater of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps. NaNs
        are ignored. If only NaNs are encountered, the negative [infinity]
        value will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val min : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [min ?n ?ofsx ?incx x] computes the smaller of the [n] elements
        in vector [x] (2-norm), separated by [incx] incremental steps.
        NaNs are ignored. If only NaNs are encountered, the [infinity] value
        will be returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sum : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [sum ?n ?ofsx ?incx x] computes the sum of the [n] elements in
        vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val prod : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> num_type
    (** [prod ?n ?ofsx ?incx x] computes the product of the [n] elements
        in vector [x], separated by [incx] incremental steps.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1 *)

    val sqr_nrm2 :
      ?stable : bool -> ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
    (** [sqr_nrm2 ?stable ?n ?c ?ofsx ?incx x] computes the square of
        the 2-norm (Euclidean norm) of vector [x] separated by [incx]
        incremental steps.  If [stable] is true, this is equivalent to
        squaring the result of calling the BLAS-function [nrm2], which
        avoids over- and underflow if possible.  If [stable] is false
        (default), [dot] will be called instead for greatly improved
        performance.

        @param stable default = [false]
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
    *)

    val ssqr :
      ?n : int ->
      ?c : num_type ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> num_type
    (** [ssqr ?n ?c ?ofsx ?incx x] computes the sum of squared differences
        of the [n] elements in vector [x] from constant [c], separated
        by [incx] incremental steps.  Please do not confuse with
        {!sqr_nrm2}!  The current function behaves differently with
        complex numbers when zero is passed in for [c].  It computes
        the square for each entry then, whereas {!sqr_nrm2} uses the
        conjugate transpose in the product.  The latter will therefore
        always return a real number.

        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param c default = zero
        @param ofsx default = 1
        @param incx default = 1
    *)


    (** {6 Operations on two vectors} *)

    val neg :
      ?n : int ->
      ?ofsy : int ->
      ?incy : int ->
      ?y : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec
      -> vec
    (** [neg ?n ?ofsy ?incy ?y ?ofsx ?incx x] negates [n] elements of the
        vector [x] using [incx] as incremental steps.   If [y] is given,
        the result will be stored in there using increments of [incy],
        otherwise a fresh vector will be used.  The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsy default = 1
        @param incy default = 1
        @param y default = fresh vector with [ofsy+(n - 1)(abs incy)] rows
        @param ofsx default = 1
        @param incx default = 1 *)

    val add :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [add ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] adds [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val sub :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [sub ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] subtracts [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val mul :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [mul ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] multiplies
        [n] elements of vectors [x] and [y] elementwise, using [incx]
        and [incy] as incremental steps respectively. If [z] is given, the
        result will be stored in there using increments of [incz], otherwise
        a fresh vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val div :
      ?n : int ->
      ?ofsz : int ->
      ?incz : int ->
      ?z : vec ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> vec
    (** [div ?n ?ofsz ?incz ?z ?ofsx ?incx x ?ofsy ?incy y] divides [n]
        elements of vectors [x] and [y] elementwise, using [incx] and [incy]
        as incremental steps respectively. If [z] is given, the result will
        be stored in there using increments of [incz], otherwise a fresh
        vector will be used. The resulting vector is returned.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsz default = 1
        @param incz default = 1
        @param z default = fresh vector with [ofsz+(n - 1)(abs incz)] rows
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

    val ssqr_diff :
      ?n : int ->
      ?ofsx : int ->
      ?incx : int ->
      vec ->
      ?ofsy : int ->
      ?incy : int ->
      vec
      -> num_type
    (** [ssqr_diff ?n ?ofsx ?incx x ?ofsy ?incy y] returns the sum of
        squared differences of [n] elements of vectors [x] and [y], using
        [incx] and [incy] as incremental steps respectively.
        @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
        @param ofsx default = 1
        @param incx default = 1
        @param ofsy default = 1
        @param incy default = 1 *)

  end

  module Mat : sig
    (** {5 Matrix operations} *)

    (** {6 Creation of matrices} *)

    val random :
      ?rnd_state : Random.State.t ->
      ?re_from : float -> ?re_range : float ->
      ?im_from : float -> ?im_range : float ->
      int -> int
      -> mat
    (** [random ?rnd_state ?re_from ?re_range ?im_from ?im_range m n]
        @return an [m]x[n] matrix initialized with random elements sampled
        uniformly from [re_range] and [im_range] starting at [re_from] and
        [im_from] for real and imaginary numbers respectively.  A random state
        [rnd_state] can be passed.

        @param rnd_state default = Random.get_state ()
        @param re_from default = -1.0
        @param re_range default = 2.0
        @param im_from default = -1.0
        @param im_range default = 2.0 *)

    open Common
    (** {6 Creation of matrices and accessors} *)

    val create : int -> int -> mat
    (** [create m n] @return a matrix containing [m] rows and [n] columns. *)

    val make : int -> int -> num_type -> mat
    (** [make m n x] @return a matrix containing [m] rows and [n] columns
        initialized with value [x]. *)

    val make0 : int -> int -> mat
    (** [make0 m n x] @return a matrix containing [m] rows and [n] columns
        initialized with the zero element. *)

    val of_array : num_type array array -> mat
    (** [of_array ar] @return a matrix initialized from the array of arrays
        [ar].  It is assumed that the OCaml matrix is in row major order
        (standard). *)

    val to_array : mat -> num_type array array
    (** [to_array mat] @return an array of arrays initialized from matrix
        [mat]. *)

    val of_col_vecs : vec array -> mat
    (** [of_col_vecs ar] @return a matrix whose columns are initialized from
        the array of vectors [ar].  The vectors must be of same length. *)

    val to_col_vecs : mat -> vec array
    (** [to_col_vecs mat] @return an array of column vectors initialized
        from matrix [mat]. *)

    val as_vec : mat -> vec
    (** [as_vec mat] @return a vector containing all elements of the
        matrix in column-major order.  The data is shared. *)

    val init_rows : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed row-wise. *)

    val init_cols : int -> int -> (int -> int -> num_type) -> mat
    (** [init_cols m n f] @return a matrix containing [m] rows and [n]
        columns, where each element at [row] and [col] is initialized by the
        result of calling [f row col]. The elements are passed column-wise. *)

    val create_mvec : int -> mat
    (** [create_mvec m] @return a matrix with one column containing [m] rows. *)

    val make_mvec : int -> num_type -> mat
    (** [make_mvec m x] @return a matrix with one column containing [m] rows
        initialized with value [x]. *)

    val mvec_of_array : num_type array -> mat
    (** [mvec_of_array ar] @return a matrix with one column
        initialized with values from array [ar]. *)

    val mvec_to_array : mat -> num_type array
    (** [mvec_to_array mat] @return an array initialized with values from
        the first (not necessarily only) column vector of matrix [mat]. *)

    val from_col_vec : vec -> mat
    (** [from_col_vec v] @return a matrix with one column representing vector [v].
        The data is shared. *)

    val from_row_vec : vec -> mat
    (** [from_row_vec v] @return a matrix with one row representing vector [v].
        The data is shared. *)

    val empty : mat
    (** [empty], the empty matrix. *)

    val identity : int -> mat
    (** [identity n] @return the [n]x[n] identity matrix. *)

    val of_diag : vec -> mat
    (** [of_diag v] @return the diagonal matrix with diagonals elements from [v]. *)

    val dim1 : mat -> int
    (** [dim1 m] @return the first dimension of matrix [m] (number of rows). *)

    val dim2 : mat -> int
    (** [dim2 m] @return the second dimension of matrix [m] (number of columns). *)

    val col : mat -> int -> vec
    (** [col m n] @return the [n]th column of matrix [m] as a vector.
        The data is shared. *)

    val copy_row : ?vec : vec -> mat -> int -> vec
    (** [copy_row ?vec mat int] @return a copy of the [n]th row of matrix [m]
        in vector [vec].

        @param vec default = fresh vector of length [dim2 mat]
    *)


    (** {6 Matrix transformations} *)

    val transpose_copy :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?br : int -> ?bc : int -> mat ->
      unit
    (** [transpose_copy ?m ?n ?ar ?ac a ?br ?bc b] copy the transpose
        of (sub-)matrix [a] into (sub-)matrix [b].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
        @param br default = [1]
        @param bc default = [1]
    *)


    val transpose : ?m : int -> ?n : int -> ?ar : int -> ?ac : int -> mat -> mat
    (** [transpose ?m ?n ?ar ?ac aa] @return the transpose of (sub-)matrix [a].

        @param m default = [Mat.dim1 a]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val detri : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> unit
    (** [detri ?up ?n ?ar ?ac a] takes a triangular (sub-)matrix [a], i.e. one
        where only the upper (iff [up] is true) or lower triangle is defined,
        and makes it a symmetric matrix by mirroring the defined triangle
        along the diagonal.

        @param up default = [true]
        @param n default = [Mat.dim1 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val packed : ?up : bool -> ?n : int -> ?ar : int -> ?ac : int -> mat -> vec
    (** [packed ?up ?n ?ar ?ac a] @return (sub-)matrix [a] in packed
        storage format.

        @param up default = [true]
        @param n default = [Mat.dim2 a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val unpacked : ?up : bool -> ?n : int -> vec -> mat
    (** [unpacked ?up x] @return an upper or lower (depending on [up])
        triangular matrix from packed representation [vec].  The other
        triangle of the matrix will be filled with zeros.

        @param up default = [true]
        @param n default = [Vec.dim x]
    *)


    (** {6 Arithmetic and other matrix operations} *)

    val copy_diag : mat -> vec
    (** [copy_diag m] @return the diagonal of matrix [m] as a vector.
        If [m] is not a square matrix, the longest possible sequence
        of diagonal elements will be returned. *)

    val trace : mat -> num_type
    (** [trace m] @return the trace of matrix [m].  If [m] is not a
        square matrix, the sum of the longest possible sequence of
        diagonal elements will be returned. *)

    val scal :
      ?m : int -> ?n : int -> num_type -> ?ar : int -> ?ac : int -> mat -> unit
    (** [scal ?m ?n alpha ?ar ?ac a] BLAS [scal] function for (sub-)matrices. *)

    val scal_cols :
      ?m : int -> ?n : int ->
      ?ar : int -> ?ac : int -> mat ->
      ?ofs : int -> vec ->
      unit
    (** [scal_cols ?m ?n ?ar ?ac a ?ofs alphas] column-wise [scal]
        function for matrices. *)

    val scal_rows :
      ?m : int -> ?n : int ->
      ?ofs : int -> vec ->
      ?ar : int -> ?ac : int -> mat ->
      unit
    (** [scal_rows ?m ?n ?ofs alphas ?ar ?ac a] row-wise [scal]
        function for matrices. *)

    val axpy :
      ?m : int ->
      ?n : int ->
      ?alpha : num_type ->
      ?xr : int ->
      ?xc : int ->
      x : mat ->
      ?yr : int ->
      ?yc : int ->
      mat
      -> unit
    (** [axpy ?m ?n ?alpha ?xr ?xc ~x ?yr ?yc y] BLAS [axpy] function for
        matrices. *)

    val gemm_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?transa : trans3 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      vec
    (** [gemm_diag ?n ?k ?beta ?ofsy ?y ?transa ?transb ?alpha ?ar ?ac a ?br ?bc b]
        computes the diagonal of the product of the (sub-)matrices [a]
        and [b] (taking into account potential transposing), multiplying
        it with [alpha] and adding [beta] times [y], storing the result in
        [y] starting at the specified offset.  [n] elements of the diagonal
        will be computed, and [k] elements of the matrices will be part of
        the dot product associated with each diagonal element.

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param transa default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_diag :
      ?n : int ->
      ?k : int ->
      ?beta : num_type ->
      ?ofsy : int ->
      ?y : vec ->
      ?trans : trans2 ->
      ?alpha : num_type ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      vec
    (** [syrk_diag ?n ?k ?beta ?ofsy ?y ?trans ?alpha ?ar ?ac a]
        computes the diagonal of the symmetric rank-k product of the
        (sub-)matrix [a], multiplying it with [alpha] and adding [beta]
        times [y], storing the result in [y] starting at the specified
        offset.  [n] elements of the diagonal will be computed, and [k]
        elements of the matrix will be part of the dot product associated
        with each diagonal element.

        @param n default = number of rows of [a] (or tr[a])
        @param k default = number of columns of [a] (or tr[a])
        @param beta default = [0]
        @param ofsy default = [1]
        @param y default = fresh vector of size [n + ofsy - 1]
        @param trans default = [`N]
        @param alpha default = [1]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val gemm_trace :
      ?n : int ->
      ?k : int ->
      ?transa : trans3 ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?transb : trans3 ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [gemm_trace ?n ?k ?transa ?ar ?ac a ?transb ?br ?bc b] computes
        the trace of the product of the (sub-)matrices [a] and [b]
        (taking into account potential transposing).  [n] is the number
        of rows (columns) to consider in [a], and [k] the number of
        columns (rows) in [b].

        @param n default = number of rows of [a] (or tr [a]) and
                           number of columns of [b] (or tr [b])
        @param k default = number of columns of [a] (or tr [a]) and
                           number of rows of [b] (or tr [b])
        @param transa default = [`N]
        @param ar default = [1]
        @param ac default = [1]
        @param transb default = [`N]
        @param br default = [1]
        @param bc default = [1]
    *)

    val syrk_trace :
      ?n : int ->
      ?k : int ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      num_type
    (** [syrk_trace ?n ?k ?ar ?ac a] computes the trace of either [a' * a]
        or [a * a'], whichever is more efficient (results are identical),
        of the (sub-)matrix [a] multiplied by its own transpose.  [n]
        is the number of rows to consider in [a], and [k] the number
        of columns to consider.

        @param n default = number of rows of [a]
        @param k default = number of columns of [a]
        @param ar default = [1]
        @param ac default = [1]
    *)

    val symm2_trace :
      ?n : int ->
      ?upa : bool ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      ?upb : bool ->
      ?br : int ->
      ?bc : int ->
      mat ->
      num_type
    (** [symm2_trace ?n ?upa ?ar ?ac a ?upb ?br ?bc b] computes the
        trace of the product of the symmetric (sub-)matrices [a] and
        [b].  [n] is the number of rows and columns to consider in [a]
        and [b].

        @param n default = dimensions of [a] and [b]
        @param upa default = true (upper triangular portion of [a] is accessed)
        @param ar default = [1]
        @param ac default = [1]
        @param upb default = true (upper triangular portion of [b] is accessed)
        @param br default = [1]
        @param bc default = [1]
    *)


    (** {6 Iterators over matrices} *)

    val map :
      (num_type -> num_type) ->
      ?m : int ->
      ?n : int ->
      ?br : int ->
      ?bc : int ->
      ?b : mat ->
      ?ar : int ->
      ?ac : int ->
      mat
      -> mat
    (** [map f ?m ?n ?br ?bc ?b ?ar ?ac a]
        @return matrix with [f] applied to each element of [a].
        @param m default = number of rows of [a]
        @param n default = number of columns of [a]
        @param b default = fresh matrix of size m by n *)

    val fold_cols : ('a -> vec -> 'a) -> ?n : int -> ?ac : int -> 'a -> mat -> 'a
    (** [fold_cols f ?n ?ac acc a]
        @return accumulator resulting from folding over each column vector.
        @param ac default = 1
        @param n default = number of columns of [a] *)

  end

  val pp_num : Format.formatter -> Complex.t -> unit
  (** [pp_num ppf el] is equivalent to [fprintf ppf "(%G, %Gi)"
      el.re el.im]. *)

  val pp_vec : (Complex.t, 'a) Io.pp_vec
  (** Pretty-printer for column vectors. *)

  val pp_mat : (Complex.t, 'a) Io.pp_mat
  (** Pretty-printer for matrices. *)


  open Common
  (** {6 BLAS-1 interface} *)

  val dotu :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> num_type
  (** [dotu ?n ?ofsy ?incy y ?ofsx ?incx x] see BLAS documentation!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param ofsx default = 1
      @param incx default = 1
  *)

  val dotc :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec
    -> num_type
  (** [dotc ?n ?ofsy ?incy y ?ofsx ?incx x] see BLAS documentation!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param ofsx default = 1
      @param incx default = 1
  *)


  (** {6 LAPACK interface} *)

  val lansy_min_lwork : int -> norm4 -> int
  (** [lansy_min_lwork m norm]
      @return the minimum length of the work array used by the [lansy]-function.
      @param norm type of norm that will be computed by [lansy]
      @param n the number of columns (and rows) in the matrix *)

  val lansy :
    ?n : int ->
    ?up : bool ->
    ?norm : norm4 ->
    ?work : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [lansy ?n ?up ?norm ?work ?ar ?ac a] see LAPACK documentation!
      @param norm default = `O
      @param up default = true (reference upper triangular part of [a])
      @param n default = number of columns of matrix [a]
      @param work default = allocated work space for norm `I *)

  val gecon_min_lwork : int -> int
  (** [gecon_min_lwork n] @return the minimum length of the work array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to
               the [gecon]-function *)

  val gecon_min_lrwork : int -> int
  (** [gecon_min_lrwork n] @return the minimum length of the rwork array
      used by the [gecon]-function.
      @param n the logical dimensions of the matrix given to [gecon]-function *)

  val gecon :
    ?n : int ->
    ?norm : norm2 ->
    ?anorm : float ->
    ?work : vec ->
    ?rwork : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [gecon ?n ?norm ?anorm ?work ?rwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of matrix [a]
      @param n default = available number of columns of matrix [a]
      @param norm default = 1-norm
      @param anorm default = norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace
      @param rwork default = automatically allocated workspace
      @param ar default = 1
      @param ac default = 1 *)

  val sycon_min_lwork : int -> int
  (** [sycon_min_lwork n] @return the minimum length of the work array
      used by the [sycon]-function.
      @param n the logical dimensions of the matrix given to
               the [sycon]-function *)

  val sycon :
      ?n : int ->
      ?up : bool ->
      ?ipiv : int_vec ->
      ?anorm : float ->
      ?work : vec ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      float
  (** [sycon ?n ?up ?ipiv ?anorm ?work ?ar ?ac a]
      @return estimate of the reciprocal of the
              condition number of symmetric matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of the factorization of [a] is stored
      @param ipiv default = vec of length [n]
      @param anorm default = 1-norm of the matrix [a] as returned by [lange]
      @param work default = automatically allocated workspace *)

  val pocon_min_lwork : int -> int
  (** [pocon_min_lwork n] @return the minimum length of the work array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to
               the [pocon]-function *)

  val pocon_min_lrwork : int -> int
  (** [pocon_min_lrwork n] @return the minimum length of the rwork array
      used by the [pocon]-function.
      @param n the logical dimensions of the matrix given to [pocon]-function *)

  val pocon :
      ?n : int ->
      ?up : bool ->
      ?anorm : float ->
      ?work : vec ->
      ?rwork : rvec ->
      ?ar : int ->
      ?ac : int ->
      mat ->
      float
  (** [pocon ?n ?up ?anorm ?work ?rwork ?ar ?ac a]
      @return estimate of the reciprocal of the condition number of
              complex Hermitian positive definite matrix [a]
      @param n default = available number of columns of matrix [a]
      @param up default = upper triangle of Cholesky factorization
                          of [a] is stored
      @param work default = automatically allocated workspace
      @param rwork default = automatically allocated workspace
      @param anorm default = 1-norm of the matrix [a] as returned by [lange] *)


  (** {7 General SVD routines} *)

  val gesvd_min_lwork : m : int -> n : int -> int
  (** [gesvd_min_lwork ~m ~n] @return the minimum length of the work array
      used by the [gesvd]-function for matrices with [m] rows and [n]
      columns. *)

  val gesvd_lrwork : m : int -> n : int -> int
  (** [gesvd_lrwork m n] @return the (minimum) length of the rwork array
      used by the [gesvd]-function. *)

  val gesvd_opt_lwork :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : rvec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?ar : int -> ?ac : int -> mat ->
    int

  val gesvd :
    ?m : int -> ?n : int ->
    ?jobu : svd_job ->
    ?jobvt : svd_job ->
    ?s : rvec ->
    ?ur : int -> ?uc : int -> ?u : mat ->
    ?vtr : int -> ?vtc : int -> ?vt : mat ->
    ?work : vec ->
    ?rwork : rvec ->
    ?ar : int -> ?ac : int -> mat ->
    rvec * mat * mat


  (** {7 General eigenvalue problem (simple drivers)} *)

  val geev_min_lwork : int -> int
  (** [geev_min_lwork n] @return the minimum length of the work array
      used by the [geev]-function.
      @param n the logical dimensions of the matrix given to [geev]-function *)

  val geev_min_lrwork : int -> int
  (** [geev_min_lrwork n] @return the minimum length of the rwork array
      used by the [geev]-function.
      @param n the logical dimensions of the matrix given to [geev]-function *)

  val geev_opt_lwork :
    ?n : int ->
    ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofsw : int -> ?w : vec ->
    ?ar : int -> ?ac : int -> mat ->
    int
   (** [geev ?work ?rwork ?n ?vlr ?vlc ?vl
         ?vrr ?vrc ?vr ?ofsw w ?ar ?ac a]
      See [geev]-function for details about arguments.
      @return "optimal" work size *)

  val geev :
    ?n : int ->
    ?work : vec ->
    ?rwork : vec ->
    ?vlr : int -> ?vlc  : int -> ?vl : mat option ->
    ?vrr : int -> ?vrc : int -> ?vr : mat option ->
    ?ofsw : int -> ?w : vec ->
    ?ar : int -> ?ac : int -> mat ->
    mat * vec * mat
  (** [geev ?work ?rwork ?n
        ?vlr ?vlc ?vl
        ?vrr ?vrc ?vr
        ?ofsw w
        ?ar ?ac a]
      @return [(lv, w, rv)], where [lv] and [rv] correspond to the left and
        right eigenvectors respectively, [w] to the eigenvalues. [lv] ([rv])
        is the empty matrix if [vl] ([vr]) is set to [None].
      @raise Failure if the function fails to converge
      @param n default = available number of columns of matrix [a]
      @param work default = automatically allocated workspace
      @param rwork default = automatically allocated workspace
      @param vl default = Automatically allocated left eigenvectors.
                          Pass [None] if you do not want to compute them,
                          [Some lv] if you want to provide the storage.
                          You can set [vlr], [vlc] in the last case.
      (See LAPACK GEEV docs for details about storage of complex eigenvectors)
      @param vr default = Automatically allocated right eigenvectors.
                           Pass [None] if you do not want to compute them,
                           [Some rv] if you want to provide the storage.
                           You can set [vrr], [vrc] in the last case.
      @param w default = automatically allocate eigenvalues
      @param a the matrix whose eigensystem is computed *)

  open Common
  (** {6 BLAS-1 interface} *)

  val swap :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [swap ?n ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val scal : ?n : int -> num_type -> ?ofsx : int -> ?incx : int -> vec -> unit
  (** [scal ?n alpha ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val copy :
    ?n : int ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [copy ?n ?ofsy ?incy ?y ?ofsx ?incx x] see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = new vector with [ofsy+(n-1)(abs incy)] rows
      @param ofsx default = 1
      @param incx default = 1 *)

  val nrm2 : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> float
  (** [nrm2 ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
  *)

  val axpy :
    ?n : int ->
    ?alpha : num_type ->
    ?ofsx : int ->
    ?incx : int ->
    x : vec ->
    ?ofsy : int ->
    ?incy : int ->
    vec ->
    unit
  (** [axpy ?n ?alpha ?ofsx ?incx ~x ?ofsy ?incy y] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ofsx default = 1
      @param incx default = 1
      @param ofsy default = 1
      @param incy default = 1 *)

  val iamax : ?n : int -> ?ofsx : int -> ?incx : int -> vec -> int
  (** [iamax ?n ?ofsx ?incx x] see BLAS documentation!
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)

  val amax :
    ?n : int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    num_type
  (** [amax ?n ?ofsx ?incx x] @return the greater of the absolute
      values of the elements of the vector [x].
      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-2 interface} *)

  val gemv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type  ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gemv ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = number of available rows in matrix [a]
      @param n default = available columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val gbmv :
    ?m : int ->
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?trans : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int ->
    int ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [gbmv
        ?m ?n ?beta ?ofsy ?incy ?y ?trans ?alpha ?ar ?ac a kl ku ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param m default = same as [n] (i.e., [a] is a square matrix)
      @param n default = available number of columns in matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val symv :
    ?n : int ->
    ?beta : num_type ->
    ?ofsy : int ->
    ?incy : int ->
    ?y : vec ->
    ?up : bool ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    vec
  (** [symv ?n ?beta ?ofsy ?incy ?y ?up ?alpha ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @return vector [y], which is overwritten.
      @param n default = dimension of symmetric matrix [a]
      @param beta default = [{ re = 0.; im = 0. }]
      @param ofsy default = 1
      @param incy default = 1
      @param y default = vector with minimal required length (see BLAS)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trmv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val trsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [trsv ?n ?trans ?diag ?up ?ar ?ac a ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of triangular matrix [a]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [a] is accessed)
      @param ar default = 1
      @param ac default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpmv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpmv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)

  val tpsv :
    ?n : int ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    unit
  (** [tpsv ?n ?trans ?diag ?up ?ofsap ap ?ofsx ?incx x]
      see BLAS documentation!
      @param n default = dimension of packed triangular matrix [ap]
      @param trans default = `N
      @param diag default = false (not a unit triangular matrix)
      @param up default = true (upper triangular portion of [ap] is accessed)
      @param ofsap default = 1
      @param ofsx default = 1
      @param incx default = 1 *)


  (** {6 BLAS-3 interface} *)

  val gemm :
    ?m : int ->
    ?n : int ->
    ?k : int ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?transa : trans3 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?transb : trans3 ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [gemm ?m ?n ?k ?beta ?cr ?cc ?c ?transa ?alpha ?ar ?ac a ?transb ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [a] (or tr [a]) and [c]
      @param n default = number of columns of [b] (or tr [b]) and [c]
      @param k default = number of columns of [a] (or tr [a]) and
                         number of rows of [b] (or tr [b])
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param transa default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param transb default = `N
      @param br default = 1
      @param bc default = 1 *)

  val symm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [symm ?m ?n ?side ?up ?beta ?cr ?cc ?c ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param m default = number of rows of [c]
      @param n default = number of columns of [c]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trmm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trmm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val trsm :
    ?m : int ->
    ?n : int ->
    ?side : side ->
    ?up : bool ->
    ?transa : trans3 ->
    ?diag : diag ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    a : mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trsm ?m ?n ?side ?up ?transa ?diag ?alpha ?ar ?ac ~a ?br ?bc b]
      see BLAS documentation!
      @return matrix [b], which is overwritten.
      @param m default = number of rows of [b]
      @param n default = number of columns of [b]
      @param side default = `L (left - multiplication is [a][b])
      @param up default = true (upper triangular portion of [a] is accessed)
      @param transa default = `N
      @param diag default = `N (non-unit)
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1 *)

  val syrk :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [syrk ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1 *)

  val syr2k :
    ?n : int ->
    ?k : int ->
    ?up : bool ->
    ?beta : num_type ->
    ?cr : int ->
    ?cc : int ->
    ?c : mat ->
    ?trans : trans2 ->
    ?alpha : num_type ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?br : int ->
    ?bc : int ->
    mat ->
    mat
  (** [syr2k ?n ?k ?up ?beta ?cr ?cc ?c ?trans ?alpha ?ar ?ac a ?br ?bc b]
      see BLAS documentation!
      @return matrix [c], which is overwritten.
      @param n default = number of rows of [a] (or [a]'), [c]
      @param k default = number of columns of [a] (or [a]')
      @param up default = true (upper triangular portion of [c] is accessed)
      @param beta default = [{ re = 0.; im = 0. }]
      @param cr default = 1
      @param cc default = 1
      @param c default = matrix with minimal required dimension
      @param trans default = `N
      @param alpha default = [{ re = 1.; im = 0. }]
      @param ar default = 1
      @param ac default = 1
      @param br default = 1
      @param bc default = 1
  *)


  (** {6 LAPACK interface} *)

  (** {7 Auxiliary routines} *)

  val lacpy :
    ?uplo : [ `U | `L ] ->
    ?m : int ->
    ?n : int ->
    ?br : int ->
    ?bc : int ->
    ?b : mat ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    mat
  (** [lacpy ?uplo ?m ?n ?br ?bc ?b ?ar ?ac a] copy a (triangular)
      (sub-)matrix [a] (to an optional (sub-)matrix [b]).

      @param uplo default = whole matrix
  *)

  val lassq :
    ?n : int ->
    ?scale : float ->
    ?sumsq : float ->
    ?ofsx : int ->
    ?incx : int ->
    vec ->
    float * float
  (** [lassq ?n ?ofsx ?incx ?scale ?sumsq] @return [(scl, ssq)], where
      [scl] is a scaling factor and [ssq] the sum of squares of vector
      [x] starting at [ofs] and using increment [incx] and initial
      [scale] and [sumsq].  See LAPACK-documentation for details!

      @param n default = greater n s.t. [ofsx+(n-1)(abs incx) <= dim x]
      @param ofsx default = 1
      @param incx default = 1
      @param scale default = 0.
      @param sumsq default = 1.
  *)

  val larnv :
    ?idist : [ `Uniform0 | `Uniform1 | `Normal ] ->
    ?ofsiseed : int ->
    ?iseed : int_vec ->
    ?n : int ->
    ?ofsx : int ->
    ?x : vec ->
    unit ->
    vec
  (** [larnv ?idist ?ofsiseed ?iseed ?n ?ofsx ?x ()] @return a random
      vector with random distribution as specifified by [idist], random seed
      [iseed], vector offset [ofsx] and optional vector [x].

      @param idist default = [`Normal]
      @param ofsiseed default = [1]
      @param iseed default = integer vector of size 4 with all ones.
      @param n default = length of [x] if [x] is provided, [1] otherwise.
      @param ofsx default = [1]
      @param x default = vector of length [n] if [n] is provided.
  *)

  val lange_min_lwork : int -> norm4 -> int
  (** [lange_min_lwork m norm]
      @return the minimum length of the work array used by the [lange]-function.
      @param m the number of rows in the matrix
      @param norm type of norm that will be computed by [lange] *)

  val lange :
    ?m : int ->
    ?n : int ->
    ?norm : norm4 ->
    ?work : rvec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    float
  (** [lange ?m ?n ?norm ?work ?ar ?ac a]
      @param m default = number of rows of matrix [a]
      @param n default = number of columns of matrix [a]
      @param norm default = `O
      @param work default = allocated work space for norm `I
      @param ar default = 1
      @param ac default = 1 *)

  val lauum :
    ?up : bool ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [lauum ?up ?n ?ar ?ac a]
      @param up default = [true]
      @param n default = minimum of available number of rows/columns in matrix [a]
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (computational routines)} *)

  val getrf :
    ?m : int ->
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [getrf ?m ?n ?ipiv ?ar ?ac a] computes an LU factorization of a
      general [m]-by-[n] matrix [a] using partial pivoting with row
      interchanges.  See LAPACK documentation.
      @return [ipiv], the  pivot indices.
      @raise Failure if the matrix is singular.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ipiv = vec of length [min(m, n)]
      @param ar default = 1
      @param ac default = 1 *)

  val getrs :
    ?n : int ->
    ?ipiv : int_vec ->
    ?trans : trans3 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [getrs ?n ?ipiv ?trans ?ar ?ac a ?nrhs ?br ?bc b] solves a system
      of linear equations [a] * X = [b] or [a]' * X = [b] with a general
      [n]-by-[n] matrix [a] using the LU factorization computed by
      {!getrf}.
      Note that matrix [a] will be passed to {!getrf} if [ipiv] was not
      provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = result from [getrf] applied to [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val getri_min_lwork : int -> int
  (** [getri_min_lwork n] @return the minimum length of the
      work array used by the {!getri}-function if the matrix has [n] columns. *)

  val getri_opt_lwork :
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [getri_opt_lwork ?n ?ar ?ac a] @return the optimal size of the
      work array used by the {!getri}-function.
      @param n default = number of columns of matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val getri :
    ?n : int ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [getri ?n ?ipiv ?work ?ar ?ac a] computes the inverse of a matrix
      using the LU factorization computed by {!getrf}.  Note that matrix
      [a] will be passed to {!getrf} if [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param ipiv default = vec of length [m] from getri
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf_min_lwork : unit -> int
  (** [sytrf_min_lwork ()] @return the minimum length of the
      work array used by the {!sytrf}-function. *)

  val sytrf_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [sytrf_opt_lwork ?n ?up ?ar ?ac a] @return the optimal size of the
      work array used by the {!sytrf}-function.
      @param n default = number of columns of matrix [a]
      @param up default = true (store upper triangle in [a])
      @param a the matrix [a]
      @param ar default = 1
      @param ac default = 1 *)

  val sytrf :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int_vec
  (** [sytrf ?n ?up ?ipiv ?work ?ar ?ac a] computes the factorization of
      the real symmetric matrix [a] using the Bunch-Kaufman diagonal
      pivoting method.
      @raise Failure if D in [a] = U*D*U' or L*D*L' is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv = vec of length n
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val sytrs :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sytrs ?n ?up ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] solves a system of
      linear equations [a]*X = [b] with a real symmetric matrix [a]
      using the factorization [a] = U*D*U**T or [a] = L*D*L**T computed
      by {!sytrf}.  Note that matrix [a] will be passed to {!sytrf} if
      [ipiv] was not provided.
      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sytri_min_lwork : int -> int
  (** [sytri_min_lwork n] @return the minimum length of the
      work array used by the {!sytri}-function if the matrix has [n] columns. *)

  val sytri :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [sytri ?n ?up ?ipiv ?work ?ar ?ac a] computes the inverse of the
      real symmetric indefinite matrix [a] using the factorization [a] =
      U*D*U**T or [a] = L*D*L**T computed by {!sytrf}.  Note that matrix
      [a] will be passed to {!sytrf} if [ipiv] was not provided.

      @raise Failure if the matrix is singular.
      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ipiv default = vec of length [n] from {!sytrf}
      @param work default = vec of optimum length
      @param ar default = 1
      @param ac default = 1 *)

  val potrf :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrf ?n ?up ?ar ?ac ?jitter a] factorizes symmetric positive
      definite matrix [a] (or the designated submatrix) using Cholesky
      factorization.

      Due to rounding errors ill-conditioned matrices may actually appear
      as if they were not positive definite, thus leading to an exception.
      One remedy for this problem is to add a small [jitter] to the
      diagonal of the matrix, which will usually allow Cholesky to complete
      successfully (though at a small bias).  For extremely ill-conditioned
      matrices it is recommended to use (symmetric) eigenvalue decomposition
      instead of this function for a numerically more stable factorization.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (store upper triangle in [a])
      @param ar default = 1
      @param ac default = 1
      @param jitter default = nothing
  *)

  val potrs :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potrs ?n ?up ?ar ?ac a ?nrhs ?br ?bc ?factorize ?jitter b] solves
      a system of linear equations [a]*X = [b], where [a] is symmetric
      positive definite matrix, using the Cholesky factorization [a] =
      U**T*U or [a] = L*L**T computed by {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val potri :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    ?factorize : bool ->
    ?jitter : num_type ->
    mat ->
    unit
  (** [potri ?n ?up ?ar ?ac ?factorize ?jitter a] computes the inverse
      of the real symmetric positive definite matrix [a] using the
      Cholesky factorization [a] = U**T*U or [a] = L*L**T computed by
      {!potrf}.

      @raise Failure if the matrix is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param ar default = 1
      @param ac default = 1
      @param factorize default = true (calls {!potrf} implicitly)
      @param jitter default = nothing
  *)

  val trtrs :
    ?n : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [trtrs ?n ?up ?trans ?diag ?ar ?ac a ?nrhs ?br ?bc b] solves a
      triangular system of the form [a] * X = [b] or [a]**T * X = [n],
      where [a] is a triangular matrix of order [n], and [b] is an
      [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val tbtrs :
    ?n : int ->
    ?kd : int ->
    ?up : bool ->
    ?trans : trans3 ->
    ?diag : diag ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [tbtrs ?n ?kd ?up ?trans ?diag ?abr ?abc ab ?nrhs ?br ?bc b]
      solves a triangular system of the form [a] * X = [b] or [a]**T * X = [b],
      where [a] is a triangular band matrix of order [n], and [b] is
      an [n]-by-[nrhs] matrix.

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [ab]
      @param kd default = number of rows in matrix [ab] - 1
      @param up default = true
      @param trans default = `N
      @param diag default = `N
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1
  *)

  val trtri :
    ?n : int ->
    ?up : bool ->
    ?diag : diag ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    unit
  (** [trtri ?n ?up ?diag ?ar ?ac a] computes the inverse of a real
      upper or lower triangular matrix [a].

      @raise Failure if the matrix [a] is singular.

      @param n default = number of columns in matrix [a]
      @param up default = true (upper triangle stored in [a])
      @param diag default = `N
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    int
  (** [geqrf_opt_lwork ?m ?n ?ar ?ac a] @return the optimum
      length of the work-array used by the {!geqrf}-function given matrix
      [a] and optionally its logical dimensions [m] and [n].

      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param ar default = 1
      @param ac default = 1
  *)

  val geqrf_min_lwork : n : int -> int
  (** [geqrf_min_lwork ~n] @return the minimum length of the
      work-array used by the {!geqrf}-function if the matrix has [n]
      columns. *)

  val geqrf :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?tau : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    vec
  (** [geqrf ?m ?n ?work ?tau ?ar ?ac a] computes a QR factorization of
      a real [m]-by-[n] matrix [a].  See LAPACK documentation.

      @return [tau], the scalar factors of the elementary reflectors.
      @param m default = number of rows in matrix [a]
      @param n default = number of columns in matrix [a]
      @param work default = vec of optimum length
      @param tau default = vec of required length
      @param ar default = 1
      @param ac default = 1 *)


  (** {7 Linear equations (simple drivers)} *)

  val gesv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gesv ?n ?ipiv ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to
      a real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] matrix and X and [b] are [n]-by-[nrhs] matrices.  The
      LU decomposition with partial pivoting and row interchanges is
      used to factor [a] as [a] = P * L * U, where P is a permutation
      matrix, L is unit lower triangular, and U is upper triangular.
      The factored form of [a] is then used to solve the system of
      equations [a] * X = [b].  On exit, [b] contains the solution matrix X.

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [a]
      @param ipiv default = vec of length [n]
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gbsv :
    ?n : int ->
    ?ipiv : int_vec ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    int ->
    int ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gbsv ?n ?ipiv ?abr ?abc ab kl ku ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is a band matrix of order [n] with [kl] subdiagonals and [ku]
      superdiagonals, and X and [b] are [n]-by-[nrhs] matrices.  The LU
      decomposition with partial pivoting and row interchanges is used
      to factor [a] as [a] = L * U, where L is a product of permutation and
      unit lower triangular matrices with [kl] subdiagonals, and U is
      upper triangular with [kl+ku] superdiagonals.  The factored form of
      [a] is then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix [a] is singular.
      @param n default = available number of columns in matrix [ab]
      @param ipiv default = vec of length [n]
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gtsv :
    ?n : int ->
    ?ofsdl : int ->
    vec ->
    ?ofsd : int ->
    vec ->
    ?ofsdu : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gtsv ?n ?ofsdl dl ?ofsd d ?ofsdu du ?nrhs ?br ?bc b] solves the
      equation [a] * X = [b] where [a] is an [n]-by-[n] tridiagonal
      matrix, by Gaussian elimination with partial pivoting.  Note that
      the equation [A]'*X = [b] may be solved by interchanging the order
      of the arguments [du] and [dl].

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsdl default = 1
      @param ofsd default = 1
      @param ofsdu default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val posv :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [posv ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] computes the solution to a
      real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix and X and [b] are
      [n]-by-[nrhs] matrices.  The Cholesky decomposition is used to
      factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ppsv :
    ?n : int ->
    ?up : bool ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ppsv ?n ?up ?ofsap ap ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a] * X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The Cholesky
      decomposition is used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular matrix and L is a lower triangular
      matrix.  The factored form of [a] is then used to solve the system
      of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val pbsv :
    ?n : int ->
    ?up : bool ->
    ?kd : int ->
    ?abr : int ->
    ?abc : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [pbsv ?n ?up ?kd ?abr ?abc ab ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an [n]-by-[n] symmetric positive definite band matrix and X
      and [b] are [n]-by-[nrhs] matrices.  The Cholesky decomposition is
      used to factor [a] as
      [a] = U**T * U,  if [up = true], or
      [a] = L * L**T,  if [up = false],
      where U is an upper triangular band matrix, and L is a lower
      triangular band matrix, with the same number of superdiagonals or
      subdiagonals as [a].  The factored form of [a] is then used to
      solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [ab]
      @param up default = true i.e., upper triangle of [ab] is stored
      @param kd default = available number of rows in matrix [ab] - 1
      @param abr default = 1
      @param abc default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val ptsv :
    ?n : int ->
    ?ofsd : int ->
    vec ->
    ?ofse : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [ptsv ?n ?ofsd d ?ofse e ?nrhs ?br ?bc b] computes the solution to
      the real system of linear equations [a]*X = [b], where [a] is an
      [n]-by-[n] symmetric positive definite tridiagonal matrix, and X
      and [b] are [n]-by-[nrhs] matrices.  A is factored as [a] =
      L*D*L**T, and the factored form of [a] is then used to solve the
      system of equations.

      @raise Failure if the matrix is singular.
      @param n default = available length of vector [d]
      @param ofsd default = 1
      @param ofse default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv_opt_lwork :
    ?n : int ->
    ?up : bool ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [sysv_opt_lwork ?n ?up ?ar ?ac a ?nrhs ?br ?bc b] @return the optimum
      length of the work-array used by the [sysv]-function given matrix
      [a], optionally its logical dimension [n] and given right hand side
      matrix [b] with an optional number [nrhs] of vectors.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val sysv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?work : vec ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [sysv ?n ?up ?ipiv ?work ?ar ?ac a ?nrhs ?br ?bc b] computes the
      solution to a real system of linear equations [a] * X = [b], where
      [a] is an N-by-N symmetric matrix and X and [b] are [n]-by-[nrhs]
      matrices.  The diagonal pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, and D is symmetric and block diagonal with
      1-by-1 and 2-by-2 diagonal blocks.  The factored form of [a] is
      then used to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = available number of columns in matrix [a]
      @param up default = true i.e., upper triangle of [a] is stored
      @param ipiv default = vec of length [n]
      @param work default = vec of optimum length (-> [sysv_opt_lwork])
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val spsv :
    ?n : int ->
    ?up : bool ->
    ?ipiv : int_vec ->
    ?ofsap : int ->
    vec ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [spsv ?n ?up ?ipiv ?ofsap ap ?nrhs ?br ?bc b] computes the
      solution to the real system of linear equations [a] * X = [b],
      where [a] is an [n]-by-[n] symmetric matrix stored in packed
      format and X and [b] are [n]-by-[nrhs] matrices.  The diagonal
      pivoting method is used to factor [a] as
      [a] = U * D * U**T,  if [up = true], or
      [a] = L * D * L**T,  if [up = false],
      where U (or L) is a product of permutation and unit upper (lower)
      triangular matrices, D is symmetric and block diagonal with 1-by-1
      and 2-by-2 diagonal blocks.  The factored form of [a] is then used
      to solve the system of equations [a] * X = [b].

      @raise Failure if the matrix is singular.
      @param n default = the greater n s.t. n(n+1)/2 <= [Vec.dim ap]
      @param up default = true i.e., upper triangle of [ap] is stored
      @param ipiv default = vec of length [n]
      @param ofsap default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


  (** {7 Least squares (simple drivers)} *)

  val gels_min_lwork : m : int -> n : int -> nrhs : int -> int
  (** [gels_min_lwork ~m ~n ~nrhs] @return the minimum length of the
      work-array used by the [gels]-function if the logical dimensions
      of the matrix are [m] rows and [n] columns and if there are [nrhs]
      right hand side vectors. *)

  val gels_opt_lwork :
    ?m : int ->
    ?n : int ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    int
  (** [gels_opt_lwork ?m ?n ?trans ?ar ?ac a ?nrhs ?br ?bc b] @return
      the optimum length of the work-array used by the [gels]-function given
      matrix [a], optionally its logical dimensions [m] and [n] and given
      right hand side matrix [b] with an optional number [nrhs] of vectors.
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns in matrix [a]
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)

  val gels :
    ?m : int ->
    ?n : int ->
    ?work : vec ->
    ?trans : trans2 ->
    ?ar : int ->
    ?ac : int ->
    mat ->
    ?nrhs : int ->
    ?br : int ->
    ?bc : int ->
    mat ->
    unit
  (** [gels ?m ?n ?work ?trans ?ar ?ac a ?nrhs ?br ?bc b] see
      LAPACK documentation!
      @param m default = available number of rows in matrix [a]
      @param n default = available number of columns of matrix [a]
      @param work default = vec of optimum length (-> {!gels_opt_lwork})
      @param trans default = `N
      @param ar default = 1
      @param ac default = 1
      @param nrhs default = available number of columns in matrix [b]
      @param br default = 1
      @param bc default = 1 *)


end
