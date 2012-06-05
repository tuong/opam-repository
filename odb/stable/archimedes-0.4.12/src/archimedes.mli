
(** A 2D plotting library with various backends.

    @version 0.4.8
    @author Christophe Troestler
    @author Pierre Hauweele
    @author Fabian Pijcke
    @author Noémie Meunier
    @author Bertrand Desmons
*)

(** {2 Introduction}

    {[
    module A = Archimedes
    let vp = A.init ["graphic"; "hold"] in
    A.Axes.box vp;
    A.fx vp sin 0. 10.;
    A.close vp
    ]}

*)
(*----------------------------------------------------------------------*)
(** {2 Affine transformations} *)


(** Module implementing affine transformations and various operations
    on them. *)
module Matrix :
sig


  (** Holds an affine transformation, such as a scale, rotation, shear,
      or a combination of those. The transformation of a point (x, y) is
      given by:
      {[
      x_new = xx *. x +. xy *. y +. x0;
      y_new = yx *. x +. yy *. y +. y0;      ]} *)
  type affine = { mutable xx: float; mutable yx: float;
                  mutable xy: float; mutable yy: float;
                  mutable x0: float; mutable y0: float; }

  type t = affine

  exception Not_invertible

  val make_identity : unit -> t
  (** [make_identity()] returns the identity transformation. *)

  val make_translate : x:float -> y:float -> t
  (** [make_translate tx ty] returns a transformation that translates
      by [tx] and [ty] in the X and Y dimensions, respectively. *)

  val make_scale : x:float -> y:float -> t
  (** [make_scale sx sy] returns a transformation that scales by [sx]
      and [sy] in the X and Y dimensions, respectively. *)

  val make_rotate : angle:float -> t
  (** [make_rotate radians] returns a transformation that rotates
      by [radians]. *)

  val set_to_identity : t -> unit
  (** Sets the current transformation to the identity transformation. *)

  val copy: t -> t
  (** [copy matrix] returns a copy of [matrix]. *)

  val blit : t -> t -> unit
  (** [blit m1 m2] copies the content of [m1] into [m2]. *)

  val translate : t -> x:float -> y:float -> unit
  (** [translate m tx ty] applies a translation by [tx], [ty] to the
      transformation in [m].  The effect of the new transformation
      is to {i first} translate the coordinates by [tx] and [ty],
      then apply the original transformation to the coordinates. *)

  val scale : t -> x:float -> y:float -> unit
  (** [scale m sx sy] applies scaling by [sx], [sy] to the
      transformation in [m].  The effect of the new transformation
      is to {i first} scale the coordinates by [sx] and [sy], then
      apply the original transformation to the coordinates. *)

  val rotate : t -> angle:float -> unit
  (** [rotate m radians] applies rotation by [radians] to the
      transformation in [m].  The effect of the new transformation
      is to {i first} rotate the coordinates by [radians], then
      apply the original transformation to the coordinates. *)

  val invert : t -> unit
  (** [invert m] changes [matrix] to be the inverse of it's original
      value.  Not all transformation matrices have inverses; if the
      matrix collapses points together (it is degenerate), then it
      has no inverse and this function will raise
      {!Matrix.Not_invertible}. *)

  val det : t -> float
  (** [det m] returns the determinant of the linear part of [m].  It
      is the (signed) area that gets the unit square after
      transformation.  *)

  val mul : t -> t -> t
  (** [multiply b a] multiplies the affine transformations in [a]
      and [b] together and return the result.  The effect of the
      resulting transformation is to {i first} apply the
      transformation in [a] to the coordinates and then apply the
      transformation in [b] to the coordinates.

      BEWARE that the order of the arguments is different from
      e.g. [Cairo.Matrix.multiply]. *)

  val mul_in : t -> t -> t -> unit
  (** [mul_in c b a] computes [mul b a] and put the result in [c]. *)

  val transform_point : t -> x:float -> y:float -> float * float
  (** [transform_point m x y] transforms the point ([x], [y]) by [m]. *)

  val transform_distance : t -> dx:float -> dy:float -> float * float
  (** [transform_distance m dx dy] transforms the distance vector
      ([dx],[dy]) by [m].  This is similar to
      {!Matrix.transform_point} except that the translation
      components of the transformation are ignored.  The calculation
      of the returned vector is as follows:
      {[
      dx2 = dx1 * xx + dy1 * xy;
      dy2 = dx1 * yx + dy1 * yy;
      ]}
      Affine transformations are position invariant, so the same
      vector always transforms to the same vector.  If (x1,y1)
      transforms to (x2,y2) then (x1+dx1,y1+dy1) will transform to
      (x2+dx2,y2+dy2) for all values of dx1 and dy1.  *)

  val inv_transform_point : t -> x:float -> y:float -> float * float
  (** Makes the inverse transformation of a point. *)

  val inv_transform_distance : t -> dx:float -> dy:float -> float * float
  (** Makes the inverse transformation of a distance. *)

  val has_shear: t -> bool
  (** Tests whether the transformation has shears.  This is also the
      case if the transformation does a rotation.  *)

  (** A data structure for holding a rectangle. *)
  type rectangle = {
    x:float;   (** X coordinate of the left side of the rectangle *)
    y:float;   (** Y coordinate of the the top side of the rectangle  *)
    w:float;   (** width of the rectangle  [>= 0]. *)
    h:float;   (** height of the rectangle [>= 0]. *)
  }

  val transform_rectangle: ?dist_basepoint:bool -> t -> rectangle -> rectangle
  (** Transformation of rectangles. This returns the smallest
      rectangle containing the transformation of the rectangle argument
      by the matrix. The optional argument [dist_basepoint] has the
      following meaning:

      - Not specified: transform the base point as a point.
      - Specified as [true]: transform the base point as a distance.
      - Specified as [false]: no transformation of the base point.*)


  (** Transformations that are the composition of translations and
      inhomogeneous dilations (different scaling factors are allowed in
      each canonical direction). *)
  module Homothety :
  sig
    type t
    (** See {!Matrix.t}, setting [xy = 0 = yx]. *)

    val of_matrix : affine -> t
    (** [of_matrix m] returns a copy of the transformation [m] if it
        contains no rotation or raise [Invalid_argument] otherwise. *)

    val to_matrix : t -> affine
    (** [to_matrix m] returns a copy of the transformation [m]. *)

    val make_identity : unit -> t
    (** See {!Matrix.make_identity}. *)
    val make_translate : x:float -> y:float -> t
    (** See {!Matrix.make_translate}. *)
    val make_scale : x:float -> y:float -> t
    (** See {!Matrix.make_scale}. *)
    val set_to_identity : t -> unit
    (** See {!Matrix.set_to_identity}. *)
    val copy: t -> t
    (** See {!Matrix.copy}. *)
    val blit : t -> t -> unit
    (** See {!Matrix.blit}. *)
    val translate : t -> x:float -> y:float -> unit
    (** See {!Matrix.translate}. *)
    val scale : t -> x:float -> y:float -> unit
    (** See {!Matrix.scale}. *)
    val invert : t -> unit
    (** See {!Matrix.invert}. *)
    val det : t -> float
    (** See {!Matrix.det}. *)

    val mul : t -> t -> t
    (** See {!Matrix.mul}. *)
    val mul_in : t -> t -> t -> unit
    (** See {!Matrix.mul_in}. *)
    val transform_point : t -> x:float -> y:float -> float * float
    (** See {!Matrix.transform_point}. *)
    val transform_distance : t -> dx:float -> dy:float -> float * float
    (** See {!Matrix.transform_distance}. *)
    val inv_transform_point : t -> x:float -> y:float -> float * float
    (** See {!Matrix.inv_transform_point}. *)
    val inv_transform_distance : t -> dx:float -> dy:float -> float * float
    (** See {!Matrix.inv_transform_distance}. *)

    val transform_rectangle: ?dist_basepoint:bool -> t -> rectangle -> rectangle
    (** See {!Matrix.transform_rectangle}. *)
  end


end
(*----------------------------------------------------------------------*)
(** {2 Base elements of a plot} *)


(** Abstract representation of colors (suitable for RGBA). *)
module Color :
sig

  type t
  (** Represent a color (immutable). *)

  val rgb : float -> float -> float -> t
  (** [rgb r g b] creates the color with transparency [~a], red
      component [r], green component [g] and blue component [b]. All
      values must be between [0.] and [1.]; raises [Invalid_argument]
      otherwise. *)

  val rgba : float -> float -> float -> float -> t
  (** [rgba r g b a] creates the color with transparency [~a], red
      component [r], green component [g] and blue component [b]. All values
      must be between [0.] and [1.]; raises [Invalid_argument] otherwise.*)

  val int : int -> t
  (** [int c] returns a color from its specification as an integer whose
      value is [0xRRGGBB] where [R], [G] and [B] are hexadecimal
      digits giving the red, green, and blue components of that color.

      It is the form used by [Graphics]. *)

  val hue : float -> t
  (** [hue h] returns a color of given hue [h] in the interval \[0 : 360.\[
      and of maximal luminance. *)

  val r : t -> float
  (** Returns the red component of a color.*)

  val g : t -> float
  (** Returns the green component of a color.*)

  val b : t -> float
  (** Returns the blue component of a color.*)

  val a : t -> float
  (** Returns the transparency (alpha) component of a color.*)

  val get_rgb : t -> float * float * float
  (** Equivalent to ([r t],[g t],[b t]).*)

  val get_rgba : t -> float * float * float * float
  (** Equivalent to ([r t],[g t],[b t], [a t]).*)

  val luminance : t -> float
  (** @return the luminance of the color.  See
      e.g. {{:http://en.wikipedia.org/wiki/Luminance_%28relative%29}Wikipedia}.
  *)

  (** {3 Predefined colors} *)

  val black : t
  val red : t
  val green : t
  val blue : t
  val yellow : t
  val magenta : t
  val cyan : t
  val white : t
  val dark_slate_grey : t

  (** {4 Shades of Blue} *)

  val deep_sky_blue : t
  val dodger_blue : t
  val aquamarine : t
  val light_blue : t
  val medium_blue : t
  val navy_blue : t
  val royal_blue : t

  (** {4 Shades of Brown} *)

  val burlywood : t
  val chocolate : t
  val tan : t

  (** {4 Shades of Green} *)

  val dark_green : t
  val dark_olive_green : t
  val forest_green : t
  val green_yellow : t
  val sea_green : t

  (** {4 Shades of Orange} *)

  val dark_orange : t
  val peach_puff : t
  val coral : t
  val orange : t

  (** {4 Shades of Red} *)

  val hot_pink : t
  val indian_red : t
  val light_pink : t
  val misty_rose : t
  val orange_red : t
  val firebrick : t

  (** {4 Shades of Violet} *)

  val dark_orchid : t
  val lavender_blush : t
  val plum : t
  val orchid : t
  val purple : t
  val thistle : t

  (** {4 Shades of White} *)

  val antique_white : t
  val old_lace : t
  val ivory : t
  val linen : t
  val wheat : t
  val white_smoke : t

  (** {4 Shades of Yellow} *)

  val lemon_chiffon : t
  val light_goldenrod : t
  val cornsilk : t
  val gold : t

  (** {4 Shades of black} *)

  val light_gray : t
  val gainsboro : t
  val silver : t
  val trolley_grey : t


  (** {3 Merging colors} *)

  (** Different ways of merging colors.  See
      http://cairographics.org/operators/ for more explanations.*)
  type operator =
  | Over (** Transparency and color components are mixed in such a way
             that it corresponds to putting the second color over the first*)
  | Source (** First color completely ignored. *)
  | Clear (** Inhibits all colors *)
  | In (** RGB components as the second color, A component product of
           the two A components. So, a transparent color result if the
           first one was transparent.*)
  | Out (** RGB components as the second color, A component product of
            the second A component with (1 - A) first component. So, a
            transparent color result if the first one was opaque.*)
  | Atop (** Transparency of the first color is the final transparency;
             mixes RGB components.*)
  | Dest (** Second color completely ignored. (<-> SOURCE)*)
  | Dest_Over (** Transparency and color components are mixed in such a
                  way that it corresponds to putting the first color over the
                  second. (<-> OVER)*)
  | Dest_In (** RGB components as the first color, A component product of
                the two A components. So, a transparent color result if the
                second one was transparent. (<-> IN)*)
  | Dest_Out (** RGB components as the first color, A component product
                 of the first A component with (1 - A) second
                 component. So, a transparent color result if the
                 second one was opaque. (<-> OUT)*)
  | Dest_Atop (** Transparency of the second color is the final transparency;
                  mixes RGB components. (<-> ATOP)*)
  | Xor (** Same mix of color than OVER, but transparency will be more
            important.*)
  | Add (** RGB components: ponderated sum of RGB components, with
            transparency. Resulting A is the sum of transparencies
            (bounded to 1. if necessary).*)
  | Saturate (** Same as ADD, but the sum for RGB components shrinks
                 the ponderation the first color components (coeff:
                 min (first A, 1 - second A)) *)


  val add : ?op:operator -> t -> t -> t
  (** Adds the first color to the second color, according to the
      operator [op] (default : [Over]).*)



end



(** Creating abstract paths. *)
module Path :
sig
  type t = Archimedes_internals.Path.t
  (** Abstract mutable path. *)

  val make: unit -> t
  (** [make ()] creates a new empty path. *)

  val is_empty : t -> bool
  (** Tells whether the path is empty. *)

  val current_point : t -> float * float
  (** Returns the point where the path currently ends. *)

  val copy: t -> t
  (** [copy p] copies a path *)

  val clear: t -> unit
  (** [clear p] clears the path (removes all operations). *)

  val extents: t -> Matrix.rectangle
  (** [extents p] returns the path's extents. *)

  val move_to: t -> x:float -> y:float -> unit
  (** [move_to p x y] moves the path current point to ([x], [y]) if both
      [x] and [y] are finite.  Otherwise, does nothing.  *)

  val rel_move_to: t -> x:float -> y:float -> unit
  (** [rel_move_to p x y] shifts the path's current point of [x]
      horizontally and [y] vertically, provided both [x] and [y] are
      finite.  Otherwise does nothing. *)

  val line_to: t -> x:float -> y:float -> unit
  (** [line_to p x y] draws a line from the path's current point to
      ([x], [y]) and sets the current point to ([x], [y]), provided both
      [x] and [y] are finite.  Otherwise does nothing. *)

  val rel_line_to: t -> x:float -> y:float -> unit
  (** [rel_line_to p x y] shifts the path's current point of [x]
      horizontally and [y] vertically and draws a line between the
      current and the new point, provided both [x] and [y] are finite.
      Otherwise does nothing. *)

  val line_of_array: t -> ?i0:int -> ?i1:int ->
    ?const_x:bool -> float array -> ?const_y:bool -> float array -> unit
  (** [line_of_array p x y] continue the current line (or start a new
      one) with the line formed by joining the points [x.(i), y.(i)],
      [i=i0,...,i1] (with possibly [i0 > i1] to indicate that the
      indices must be followed in decreasing order).  Points with at
      least one NaN or infinite coordinate are skipped when they are at
      the beginning or the end and result in a {!move_to} when
      separating finite points.

      @param const_x by setting it to [true], you indicate that you will
      not modify [x] (so it will not be copied).  Default: false.
      @param const_y Same as [const_x] but for [y].
      @param i0 start index.  Default: [0].
      @param i1 last index.  Default: [Array.length x - 1].
      @raise Failure if [y] is to small to possess the indices [i0 .. i1]. *)

  type vec =
    (float, Bigarray.float64_elt, Bigarray.fortran_layout) Bigarray.Array1.t

  val line_of_vec: t -> ?i0:int -> ?i1:int ->
    ?const_x:bool -> vec -> ?const_y:bool -> vec -> unit
  (** Same as {!line_of_array} but for FORTRAN bigarrays. *)

  type cvec = (float, Bigarray.float64_elt, Bigarray.c_layout) Bigarray.Array1.t

  val line_of_cvec: t -> ?i0:int -> ?i1:int ->
    ?const_x:bool -> cvec -> ?const_y:bool -> cvec -> unit
  (** Same as {!line_of_array} but for C bigarrays. *)


  val rectangle: t -> x:float -> y:float -> w:float -> h:float -> unit
  (** [rectangle p x y w h] draws a rectangle specified by ([x], [y],
      [w], [h]) if all four quantities [x], [y], [w] and [h] are finite.
      Does nothing otherwise. *)

  val curve_to: t -> x1:float -> y1:float -> x2:float -> y2:float ->
    x3:float -> y3:float -> unit
  (** [curve_to p x1 y1 x2 y2 x3 y3] draws a cubic Bezier curve using
      the path's current point as first point (x0, y0) if it is set,
      else ([x1, y1]).  Sets the path's current point to ([x3], [y3]).
      The above holds provided all values [x1], [y1], [x2], [y2], [x3],
      and [y3] are finite.  The function does nothing otherwise. *)

  val arc: t -> r:float -> a1:float -> a2:float -> unit
  (** [arc p r a1 a2] draws an arc starting at the path's current
      point. The starting angle is [a1], the radius [r] and the arc is
      drawn clockwise to the angle [a2]. The angles are given in
      radians.  The above holds provided the three values [r], [a1], and
      [a2] are finite.  The function does nothing otherwise.  *)

  val close: t -> unit
  (** [close p] Closes the path. It is usually not required to close a
      path, this is useful only to ensure the path won't be extended. *)

  val current_point: t -> float * float
  (** [current_point p] returns the current point of the path.
      @raise Failure if no current point exists. *)

  val append: t -> t -> unit
  (** [append p1 p2] append the path [p2] to the end of [p1] and {i
      clear} [p2].  The current point of [p1] becomes the one of
      [p2]. *)

  val transform : Matrix.t -> t -> t
  (** [transform m p] returns a new path resulting from applying the
      affine transformation [m] to [p]. *)

end
(*----------------------------------------------------------------------*)
(** {2 Registering backends} *)


(** Module providing a uniform interface and managing the dynamic
    loading of the backends.  This modules is only useful to create
    new backends and should not be used for plotting data. *)
module Backend :
sig

  type line_cap =
  | BUTT  (** start(stop) the line exactly at the start(end) point *)
  | ROUND (** use a round ending, the center of the circle is the end point *)
  | SQUARE (** use squared ending, the center of the square is the end point *)

  type line_join =
  | JOIN_MITER (** use a sharp (angled) corner *)
  | JOIN_ROUND (** use a rounded join, the center of the circle is the
                   joint point *)
  | JOIN_BEVEL (** use a cut-off join, the join is cut off at half the line
                   width from the joint point *)

  type text_position =
  | CC  (** centrer horizontally and vertically *)
  | LC  (** align left horizontally and center vertically *)
  | RC  (** align right horizontally and center vertically *)
  | CT  (** center horizontally and align top vertically *)
  | CB  (** center horizontally and align bottom vertically *)
  | LT  (** align left horizontally and top vertically *)
  | LB  (** align left horizontally and bottom vertically *)
  | RT  (** align right horizontally and top vertically *)
  | RB  (** align right horizontally and bottom vertically *)


  (** Specifies variants of a font face based on their slant. *)
  type slant = Upright | Italic

  (** Specifies variants of a font face based on their weight. *)
  type weight = Normal | Bold

  (** The interface that backends must provide to be registered. *)
  module type T =
  sig
    type t
    (** Handle to a backend. *)

    val set_color : t -> Color.t -> unit
    (** [set_color bk c] sets the color of the backend [bk] to [c]. *)
    val set_line_width : t -> float -> unit
    (** [set_line_width bk w] sets the line width of the backend [bk] to
        [w].  The line width is expressed in the natural backend
        coordinates (i.e. when the CTM is the identity). *)
    val set_line_cap : t -> line_cap -> unit
    (** [set_line_cap bk c] sets the line cap for the backend [bk] to [c]. *)
    val set_dash : t -> float -> float array -> unit
    (** [set_dash bk ofs pattern] *)
    val set_line_join : t -> line_join -> unit
    (** [set_line_join bk j] sets the line join for the backend [bk]
        to [j]. *)

    val get_line_width: t -> float
    val get_line_cap: t -> line_cap
    val get_dash: t -> float array * float
    val get_line_join: t -> line_join

    val move_to : t -> x:float -> y:float -> unit
    (** Begin a new sub-path.  After this call the current point will be
        [(x, y)]. *)
    val line_to : t -> x:float -> y:float -> unit
    (** [line_to bk x y] Adds a line to the path from the current point
        to position [(x, y)] in the current backend coordinates.  After
        this call the current point will be [(x, y)].

        If there is no current point before the call to [line_to] this
        function will behave as {!move_to}[ bk x y]. *)

    val rel_move_to : t -> x:float -> y:float -> unit
    val rel_line_to : t -> x:float -> y:float -> unit

    val curve_to : t ->
      x1:float -> y1:float ->
      x2:float -> y2:float ->
      x3:float -> y3:float -> unit
    (** [curve_to bk x1 y1 x2 y2 x3 y3] adds an Bezier curve to the
        path, starting at the current point, ending at point
        [(x3,y3)], with control points [(x1,y1)] and [(x2,y2)]. *)

    val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (** [rectangle bk x y w h] adds to the current path of [bk] a
        rectangle whose lower left corner is at [(x,y)] and width
        and height are respectively [w] and [h]. *)

    val arc : t -> r:float -> a1:float -> a2:float -> unit
    (** [arc bk r a1 a2] add an arc to the current path starting from
        the current point with a radius [r], starting at angle [a1]
        and going clockwise to angle [a2]. *)

    val close_path : t -> unit
    (** Adds a line segment to the path from the current point to
        the beginning of the current sub-path (the most recent point
        passed to {!Archimedes.Backend.T.move_to}) and closes this
        sub-path. *)
    val clear_path : t -> unit
    (** Clears the current path. After this call there will be no
        path.  Nothing is guaranteed about the current point (it may
        not be preserved). *)
    val path_extents : t -> Matrix.rectangle

    val stroke : t -> unit
    (** [stroke bk] draw the curve described by the current path
        according to the current line width and color.  *)
    val stroke_preserve : t -> unit
    (** Same as {!stroke} but make sure the current path is unmodified. *)
    val fill : t -> unit
    (** [fill bk] draw the curve described by the current path according
        to the current line width and color.  The current path may be
        modified.  This is affected by the CTM.  *)
    val fill_preserve : t -> unit
    (** Same as {!fill} but make sure the current path is unmodified. *)

    val stroke_path_preserve : t -> Path.t -> unit
    (** [stroke_path bk p] stroke the abstract path [p], where its
        coordinates are interpreted in the current transformation
        matrix.  Of course, the current clipping, line width and color
        are be obeyed.  This function may modify the current path in
        [bk].

        For backend developers: the internal representation of the path
        is available in [Archimedes_internals.Path]. *)
    val fill_path_preserve : t -> Path.t -> unit
    (** [fill_path_preserve] is similar to [stroke_path_preserve] except
        that it fills the path. *)

    val fill_with_color : t -> Color.t -> unit
    (** [fill_with_color t c] fill the current path of [t] with the
        color [c].  Even if the color is transparent, it must {b
        replace} all underlying elements (contrarily to {!Backend.T.fill}
        which will show the underlying elements through a transparent
        color).  If transparency is not supported by the backend, it
        does the same as {!Backend.T.fill}, except that this operation
        does not change the current color of the backend.  It may modify
        the current path however. *)

    val show : t -> unit
    (** Some backends may not show immediately the action of {!stroke},
        {!fill}, {!stroke_path_preserve},... immediately (usually
        because it is expensive but also to avoid flicker during
        animations).  [show bk] forces the backend to update.  *)

    val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
    (** Establishes a new clip rectangle by intersecting the current
        clip rectangle.  This {i may clear} the current path.  Calling
        {clip_rectangle} can only make the clip region smaller, never
        larger.  For [clip_rectangle] to have only a local effect, put
        it in a {!save} / {!restore} group.

        [clip_rectangle] is garantee to respect the CTM only if the
        components [xy] and [yx] of the matrix are both [0.]. *)

    val save : t -> unit
    (** Save the current state of the backend.  Note that
        save/restore must not affect the current path. *)
    val restore : t -> unit
    (** Restore the saved state of the backend. *)

    val translate : t -> x:float -> y:float -> unit
    (** [translate cr tx ty] modifies the current transformation
        matrix by translating the user-space origin by ([tx],[ty]). *)
    val scale : t -> x:float -> y:float -> unit
    (** [scale sx sy] modifies the current transformation matrix by
        scaling the X and Y user-space axes by [sx] and [sy]
        respectively. *)
    val rotate : t -> angle:float -> unit
    (** Modifies the current transformation matrix by rotating the
        user-space axes by [angle] radians. *)
    val set_matrix : t -> Matrix.t -> unit
    (** Set the current transformation matrix which is the matrix
        transorming user to device coordinates. *)
    val get_matrix : t -> Matrix.t
    (** Return the current transformation matrix.  Modifying this
        matrix does not affect the matrix held in [t]. *)
    val flipy : t -> bool
    (** [true] iff this kind of device has its Y axis pointing
        downwards.
        FIXME: really needed ?  Beware that on some devices, the font
        display happens in the current coordinates. *)

    val select_font_face : t -> slant -> weight -> string -> unit
    (** [select_font_face t slant weight family] selects a family
        and style of font from a simplified description as a family
        name, slant and weight.  Family names are bakend dependent.
        Raise an exception if the face is not supported. *)
    val set_font_size : t -> float -> unit
    (** Set the scaling of the font. *)
    val text_extents : t -> string -> Matrix.rectangle
    (** Returns a rectangle whose width and height specify
        respectively the length and the height of the text. The x and
        y values give the lower bottom point of the rectangle as if
        the text was placed at the origin.*)
    val show_text : t -> rotate:float -> x:float -> y:float ->
      text_position -> string -> unit
        (** [show_text t angle x y pos txt] displays [txt] at the point
            ([x],[y]) as indicated by [pos].  The point ([x],[y]) is in
            the current coordinate system but the current transformation
            matrix will NOT be applied to the text itself.  [angle]
            indicates by how many radians the text must be rotated
            w.r.t. the x-axis (in the current coordinate system, assuming
            it is orthonormal) -- not all device support rotations of
            angles [<> 0.] (in device coordinates).  This is an immediate
            operation: no [stroke] nor [fill] are required (nor will have
            any effect).  *)
  end

  type error =
  | Corrupted_dependency of string
  | Non_loadable_dependency of string * Dynlink.error
  | Nonexistent of string  (** Cannot find the backend in the directories *)
  | Not_loadable of string * Dynlink.error
      (** Cannot load the backend because of the dynlink error. *)
  | Not_registering of string (** Not applying the {!Backend.Register}
                                  functor. *)

  exception Error of error * string
  (** Exception raised when a backend cannot be loaded. *)

  include T

  val make : ?dirs:string list -> string list -> float -> float -> t
  (** [make backend width height] creates a new backend of the
      given dimensions.  The units of the dimensions are backend
      dependent.

      The first element of [backend] is the name (case insensitive) of
      the underlying engine.  It may be followed by one or several
      options.  For example, ["Graphics"] for the graphics backend or
      ["Cairo"; "PNG"; filename] for the Cairo backend, using a PNG
      surface to be saved to [filename].  The empty list selects the
      graphics backend. *)

  val close : t -> unit
  (** Close the handle.  For some backends, the output will not be
      complete until this function is called. *)

  val height : t -> float
    (** Returns the width of the backend canvas. *)

  val width : t -> float
    (** Returns the height of the backend canvas. *)

  val name : t -> string
  (** Returns the name under which the backend was registered. *)

  val registered: unit -> string list
    (** Return the list of registered (i.e. loaded) backends. *)

  val available : dirs:string list -> string list
  (** Return the list of available backends in the given directories. *)


  (************************************************************************)
  (** {2 Registering new modules} *)

  module type Capabilities =
  sig
    include T

    val name : string
      (** Name under which to register the backend. *)

    val make : options:string list -> float -> float -> t
      (** [create options width height] must creates a new handle of
          size [width]×[height] (in units proper to the module) on which
          the subsequent drawing functions operate.  [options] allows to
          pass options to the backend (this is backend specific). *)

    val close : options:string list -> t -> unit
    (** Close the handle.  This function will be given the options
        specified at backend creation so it can react appropriately if
        some final work need to be done for some of them. *)
  end

  module Register(B: Capabilities) : sig end
  (** The {i side effect} of this functor application is to register
      the functions of the backend [B] under the name [B.name].

      A backend [B] must be declared in a file archimedes_[B.name]
      (compiled to a .cmo and/or .cmxs library) and the functor
      application must be executed as part of the initialisation code.
      We recommend the use of [let module U = Register(B) in ()] to
      perform the registration.  *)
end
(*----------------------------------------------------------------------*)
(** {2 Managing viewports} *)


(** Systems of coordinates (inhomogeneous homotheties) relative to
    other coordinate systems with automatic updates.  The automatic
    update refers to the fact that, if a coordinate system is upated,
    all coordinate systems which depend on it (possibly through
    several intermediate coordinate systems), they will use the
    updated version. *)
module Coordinate :
sig

  type t
  (** Mutable affine coordinate system. *)

  type ctm
  (** Current transformation matrix of the backend (to be able to
      restore it with {!Coordinate.restore}. *)

  val use : Backend.t -> t -> ctm
  (** After a call to [use b c], all backend operations will be
      performed in the coordinates [c].  It returns the current
      coordinate system so one can restore it with
      {!Coordinate.restore}. *)

  val restore : Backend.t -> ctm -> unit
  (** [restore b c] restore the coordinate transformation matrix [ctm]
      for the backend [b]. *)


  (** {2 Transforming coordinates} *)

  val to_parent : t -> x:float -> y:float -> float * float
  (** [to_parent coord x y] returns the location of the point [(x,y)]
      in parent's coordinates.*)

  val from_parent : t -> x:float -> y:float -> float * float
  (** [from_child coord x y] returns the location of the point [(x,y)]
      from parent's coordinates. *)

  val to_device : t -> x:float -> y:float -> float * float
  (** [to_device coord x y] returns the location of the point [(x,y)]
      in device coordinates.*)

  val to_device_distance : t -> dx:float -> dy:float -> float * float
  (** [to_device coord dx dy] returns the distance [(dx,dy)] in device
      coordinates (i.e. the translation in [coord] is ignored).  *)

  val to_coord : t -> x:float -> y:float -> float * float
  (** [to_coord coord x y] converts the (device) point [(x,y)] into
      the corresponding point, expressed in [coord] coordinates. *)

  val to_coord_distance : t -> dx:float -> dy:float -> float * float
  (** [to_coord coord x y] converts the (device) distance [(dx,dy)]
      into the corresponding distance, expressed in [coord]
      coordinates. *)


  (** {2 Creating new coordinate systems} *)

  val make_root : Matrix.Homothety.t -> t
  (** [make_root m] make a system of coordinates which, when used,
      amounts to use [m].  This coordinate system depends on no
      other  so will never be updated.  It can be modified however
      (the matrix [m] is copied so no modification will affect [m]). *)

  val make_identity : t -> t
  (** [make_identity coord] defines a new system of coordinates that
      initially consist in the identity transformation to [coord]. *)

  val make_translate : t -> x:float -> y:float -> t
  (** [make_translate coord x y] defines a new coordinate system that
      consists in moving the origin of [coord] to the point [(x,y)]
      (understood as coordinates in the system [coord]).  If [coord]
      is modified, the new system will be updated as well. *)

  val make_scale : t -> x:float -> y:float -> t
  (** [make_scale coord x y] defines a new coordinate system that
      consists in dilating axis X and Y of [coord] by a factor of [x]
      and [y] respectively.  If [coord] is modified, the new system
      will be updated as well. *)

  val make_from_transform : t -> Matrix.Homothety.t -> t
  (** [make_from_transform coord tm] defines a new coordinate system
      that consists first in applying [tm] and then the tranformation in
      [coord].  In other words, [tm] is the transformation from the
      desired coordinate system to [coord].  If [coord] is modified, the
      new system will be updated as well. *)

  val copy : t -> t
  (** Returns a completely independent copy of the current coordinate
      system. *)


  (** {2 Modifying this coordinate system} *)

  val translate : t -> x:float -> y:float -> unit
  (** [translate coord x y] modifies the coordinate system [coord]
      translating its origin to the point [(x,y)] (understood as
      coordinates in the system [coord]). *)

  val scale : t -> x:float -> y:float -> unit
  (** [scale coord x y] modifies the coordinate system [coord]
      dilating its axis X and Y by a factor of [x] and [y]
      respectively. *)

  val transform : t -> Matrix.t -> unit
  (** [transform coord tm] modifies the coordinate system [coord]
      changing the transformation matrix to its parent (the one it was
      created from) to [tm]. *)


  (** {2 Monitoring coordinate systems for updates} *)

  type monitor
  (** Handle to monitor the updates to a coordinate system. *)

  val monitor : t -> monitor
  (** [monitor coord] creates a new monitor for changes to [coord]
      (initially not set). *)

  val reset : monitor -> unit
  (** [reset m] reset the monitor.  See {!Coordinate.changed}. *)

  val changed : monitor -> bool
  (** [changed m] tell whether the coordinate system [m] is attached
      to was updated (possibly because of one of the coordinate systems
      it (transitively) depends on was mofidied) since the last [reset]. *)

end



(** Area on which graphs can be made. *)
module Viewport :
sig

  type t
  (** Viewport handle. *)

  type coord_name = [`Device | `Graph | `Data | `Orthonormal]

  val get_coord_from_name : t -> coord_name -> Coordinate.t
  (** [get_coord_from_name viewport coord_name] returns one of the
      coordinate systems of the viewport *)

  (** {2 Create new viewports} *)

  val make : t -> ?lines:float -> ?text:float -> ?marks:float ->
    ?redim:(t -> float -> float -> unit) ->
    ?coord:[`Device | `Graph | `Orthonormal] ->
    float -> float -> float -> float -> t
  (** [make parent xmin xmax ymin ymax] creates and returns a viewport
      on top of [parent] with top left corner ([xmin], [ymin]) and
      bottom right corner ([xmax], [ymax]).

      @param lines see {!init}
      @param text see {!init}
      @param marks see {!init}
      @param coord the coordinate system in which to interpret [xmin],
      [xmax], [ymin], and [ymax].  Default: [`Device].
      @param redim the function to execute when the viewport is
      redimensioned.  Default: do nothing.
  *)

  val show : t -> unit
  (** [show vp] forces the viewport [vp] and all its children to
      immediately display their current content. *)

  val get_backend : t -> Backend.t
  (** [get_backend vp] returns the backend associated to [vp], if vp is
      built over another viewport, the same backend is used. *)

  val desync_ratio : t -> unit
  (** [desync_ratio vp] make [vp] single. The ratio used will be the one
      used before desync. *)

  val sync_ratio : t -> t -> unit
  (** [sync_ratio vp vp_base] synchronizes [vp]'s ratio with the
      [vp_base]'s one. *)

  val desync_range : ?x:bool -> ?y:bool -> t -> unit
  (** [desync_range vp] make [vp] single. The range used will be the one
      used before desync.

      @param x desync the x axis (default: true)

      @param y desync the y axis (default: true)
  *)

  val sync_range : ?x:bool -> ?y:bool -> t -> t -> unit
  (** [sync_range vp vp_base] synchronizes [vp]'s ranges (according
      to ?x and ?y params) with the ranges of [vp_base]. The range
      consists of a xmin and a xmax values, which defines the bounds of
      the viewport in Coordinate data.

      @param x sync the x axis (default: false, but true if neither [x] nor
      [y] are set)

      @param y sync the y axis (default: false, but true if neither [x] nor
      [y] are set)
  *)

  val desync_unit_size : ?x:bool -> ?y:bool -> t -> unit
  (** [desync_unit_size vp] make [vp] single. The unit size used will be
      the one used before desync.

      @param x desync the x axis (default: true)

      @param y desync the y axis (default: true)
  *)

  val sync_unit_size : ?x:bool -> ?y:bool -> t -> t -> unit
  (** [sync_unit_size vp vp_base] synchronizes [vp]'s unit sizes
      (according to ?x and ?y params) with the sizes of [vp_base].

      @param x sync the x axis (default: true)

      @param y sync the y axis (default: true)
  *)

  val sync : ?x:bool -> ?y:bool -> t -> t -> unit

  val grid : ?syncs:(bool * bool * bool * bool) -> t -> int -> int -> t array array
  (** [grid parent nx ny] returns [vp] an array of [nx] * [ny]
      sub-viewports of [parent]  arranged in a grid of [nx] columns and
      [ny] rows.  The bottom left viewport is [vp.(0).(0)], the one to
      its right (resp. abobve) is [vp.(1).(0)] (resp. [vp.(0).(1)]).

      @param syncs (cx, cy, rx, ry) where [cx] (resp. [cy]) says whether
      to synchronize the X-axis (resp. the [Y-axis]) along the columns
      and [rx] (resp. [ry]) says whether to synchronize the X-axis
      (resp. the Y-axis) along the rows.  Default: all [false].
  *)

  val rows : ?syncs:(bool * bool) -> t -> int -> t array
  (** [rows parent ny] returns [vp] an array of [ny] viewports arranged
      in a column, the bottom one being [vp.(0)].

      @param syncs the axes to synchronize (x, y).  Default: both [false].
  *)

  val columns : ?syncs:(bool * bool) -> t -> int -> t array
  (** [colimns parent nx] creates [n_cols] viewports layouted in a
      row and returns them in an array of viewports

      @param syncs the axes to synchronize (x, y)
  *)
  (*val fixed_left : ?axes_sys:bool -> float -> t -> viewport * viewport
    val fixed_right : ?axes_sys:bool -> float -> t -> viewport * viewport
    val fixed_top : ?axes_sys:bool -> float -> t -> viewport * viewport
    val fixed_bottom : ?axes_sys:bool -> float -> t -> viewport * viewport*)
  val layout_borders : ?north:float -> ?south:float -> ?west:float ->
    ?east:float -> t -> t * t * t * t * t
  (** [layout_borders parent] returns a 5-uple of viewports where the 4
      first viewports are fixed in size towards the center while the fifth
      one is extensible. The viewports are north, south, west, east, center
      and are placed conformally to their names.

      @param north the size of the north's viewport; if zero (default),
      this viewport is unused (the north viewport will be the same as
      the center one)

      @param south the size of the south's viewport; same behaviour as
      north if zero

      @param west the size of the west's viewport; same behaviour as
      north if zero

      @param east the size of the east's viewport; same behaviour as
      north if zero
  *)

  val ortho_from : t -> coord_name -> float * float -> float * float
  val data_from : t -> coord_name -> float * float -> float * float

  val set_line_width : t -> float -> unit
  (** [set_line_width vp w] set the absolute width of the lines on the
      viewport [vp] to [w]. Default is 1.*)

  val set_font_size : t -> float -> unit
  (** [set_font_size vp s] set the absolute font size of the viewport
      [vp] to [s]. Default is 12. *)

  val set_mark_size : t -> float -> unit
  (** [set_mark_size vp s] set the absolute mark size of the viewport
      [vp] to [s] . Default is 7 *)

  val set_rel_line_width : t -> float -> unit
  (** [set_rel_line_width vp w]. Same as set_line_width but relative
      to the viewport. *)

  val set_rel_font_size : t -> float -> unit
  (** [set_rel_font_size vp s]. Same as set_font_size but relative to
      the viewport *)

  val set_rel_mark_size : t -> float -> unit
  (** [set_rel_mark_size vp s] Same as set_mark_size but relative to
      the viewport *)

  val get_color : t -> Color.t
  (** [get_color vp] return the current tracing color of the viewport
      [vp] *)

  val get_background_color : t -> Color.t
  (** [get_background_color vp] return the current background color of
      the viewport [vp] *)

  val get_line_width : t -> float
  (** [get_line_width vp] return the current width of the lines on the
      viewport [vp] *)

  val get_font_size : t -> float
  (** [get_font_size vp] return the current size of the font on the
      viewport [vp] *)

  val get_mark_size : t -> float
  (** [get_mark_size vp] return the current size of the marks on the
      viewport [vp] *)

  val lower_left_corner  : t -> float * float
  (** The device's coordinates of the viewport's lower left corner *)

  val upper_right_corner : t -> float * float
  (** The device's coordinates of the viewport's upper right corner *)

  val dimensions : t -> float * float
  (** The device's width and height of the viewport *)

  val set_color : t -> Color.t -> unit
  (** [set_color vp c] change the color of the elements in the
      viewport [vp] to the color [c] *)

  val set_global_line_cap : t -> Backend.line_cap -> unit
  val set_global_dash : t -> float -> float array -> unit
  val set_global_line_join : t -> Backend.line_join -> unit
  val get_line_cap : t -> Backend.line_cap
  val get_dash : t -> float array * float
  val get_line_join : t -> Backend.line_join
  val move_to : t -> x:float -> y:float -> unit
  val line_to : t -> x:float -> y:float -> unit
  val rel_move_to : t -> x:float -> y:float -> unit
  val rel_line_to : t -> x:float -> y:float -> unit
  val curve_to : t ->
    x1:float -> y1:float -> x2:float -> y2:float -> x3:float -> y3:float -> unit
  val rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  val arc : t -> r:float -> a1:float -> a2:float -> unit
  val close_path : t -> unit
  val clear_path : t -> unit
  (*val path_extents : t -> rectangle*)
  val stroke_preserve : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  (** strokes the path (default: viewport's path) on the specified
      coordinate system, doesn't clear the viewport's path if no path
      given *)
  val stroke : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  (** strokes the path (default: viewport's path) on the specified
      coordinate system, does clear the viewport's path if no path given *)
  val fill_preserve : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  val fill : ?path:Path.t -> ?fit:bool -> t -> coord_name -> unit
  val set_clip : t -> bool -> unit
  (** [set_clip vp c] whether to enable or disable clipping for every
      following instructions on [vp]. *)
  val clip_rectangle : t -> x:float -> y:float -> w:float -> h:float -> unit
  val select_font_face : t -> Backend.slant -> Backend.weight -> string -> unit

  val text :
    t -> ?coord:coord_name ->
    ?rotate:float ->
    float -> float -> ?pos:Backend.text_position -> string -> unit
  (** [text vp x y s] display the string [s] at position [(x, y)].

      @param coord the coordinate system in which the position [(x,y)]
      has to be understood.  Default: [Data].
      @param rotate the angle (in radian) that the text must be rotated.
      Default: [0.].
      @param pos the position of the text [s] w.r.t. the position
      [(x,y)].  Default: centering both horizontally and vertically. *)


  val mark : t -> x:float -> y:float -> string -> unit
  (** [mark vp x y m] draw the mark given by [m] on the viewport [vp] at
      position [(x,y)] if both [x] and [y] are finite.  Otherwise, does
      nothing. *)


  val axes_ratio : t -> float -> unit
  (** [axes_ratio vp ratio] forces axes to keep [ratio] ([w / h]). *)
  val xrange : t -> float -> float -> unit
  (** [xrange vp xmin xmax] set the OX interval of the viewport [vp] from
      [xmin] to [xmax] *)
  val yrange : t -> float -> float -> unit
  (** [yrange vp ymin ymax] set the OY interval of the viewport [vp]
      from [xmin] to [xmax] *)
  val xlabel : t -> string -> unit
  (** [xlabel vp label] set the OX representation to [label] for the
      viewport [vp] *)
  val ylabel : t -> string -> unit
  (** [ylabel vp label] set the OY representation to [label] for the
      viewport [vp] *)
  val title : t -> string -> unit
  (** [title vp t] set the title [t] above the viewport [vp] *)

  val xmin : t -> float
  (** [xmin vp] return the [xmin] of the range on the viewport [vp] *)
  val xmax : t -> float
  (** [xmax vp] return the [xmax] of the range on the viewport [vp] *)
  val ymin : t -> float
  (** [ymin vp] return the [ymin] of the range on the viewport [vp] *)
  val ymax : t -> float
  (** [ymax vp] return the [ymax] of the range on the viewport [vp] *)

  val xlog : t -> bool
  (** [xlog vp] return true if OX is in log scale on the viewport [vp] *)
  val ylog : t -> bool
  (** [ylog vp] return true if OY is in log scale on the viewport [vp] *)
  val set_xlog : t -> bool -> unit
  (** [set_xlog vp true] set a log scale on OX on the viewport [vp] *)
  val set_ylog : t -> bool -> unit
  (** [set_ylog vp true] set a log scale on OY on the viewport [vp] *)

  val set_line_width_direct : t -> float -> unit -> unit
  val set_font_size_direct : t -> float -> unit -> unit
  val set_mark_size_direct : t -> float -> unit -> unit
  val set_rel_line_width_direct : t -> float -> unit -> unit
  val set_rel_font_size_direct : t -> float -> unit -> unit
  val set_rel_mark_size_direct : t -> float -> unit -> unit
  val set_color_direct : t -> Color.t -> unit -> unit
  val set_line_cap_direct : t -> Backend.line_cap -> unit -> unit
  val set_dash_direct : t -> float -> float array -> unit -> unit
  val set_line_join_direct : t -> Backend.line_join -> unit -> unit
  val stroke_direct : ?path:Path.t -> t -> coord_name -> unit -> unit
  val fill_direct : ?path:Path.t -> t -> coord_name -> unit -> unit
  val clip_rectangle_direct : t -> x:float -> y:float -> w:float ->
    h:float -> unit -> unit
  val select_font_face_direct : t -> Backend.slant -> Backend.weight ->
    string -> unit -> unit
  val show_text_direct : t -> coord_name -> ?rotate:float ->
    x:float -> y:float -> Backend.text_position -> string -> unit -> unit
  val mark_direct : t -> x:float -> y:float -> string -> unit -> unit
  val save_direct : t -> unit -> unit
  val restore_direct : t -> unit -> unit


  val add_instruction : t -> (unit -> unit) -> unit
  val do_instructions : t -> unit

  val remove_last_instruction : t -> unit
  val clear_instructions : t -> unit

  val auto_fit : t -> float -> float -> float -> float -> unit
  (** [auto_fit vp x0 y0 x1 y1] ensures that the rectangle delimited by
      (x0, y0) and (x1, y1) is included into the axes' ranges *)

  val fit : t -> Matrix.rectangle -> unit
  (** [fit vp r] ensures that the rectangle [r] is included into the
      axes ranges. *)

  val save : t -> unit
  val restore : t -> unit

end
(*----------------------------------------------------------------------*)
(** {2 Sampling functions} *)



(** Adaptative sampling of functions. *)
module Sampler :
sig

  type strategy = float -> float -> float
  (** A strategy is a function [f t1 t2] that returns an internal point
      tm between [t1] and [t2] which will be used to decide if we need
      to increment precision or not.  The given [t1] and [t2] will
      always be finite. *)

  type cost = Matrix.rectangle ->
    float -> float -> float -> float -> float -> float -> float
  (** A cost [f bb x0 y0 xm ym x1 y1] which returns the cost measuring
      how much the three points [(x0, y0)], [(xm, ym)], and [(x1, y1)]
      differ from a straight line.  [bb] is a rough bounding box of the
      set of points that can be used to determine whether two points are
      close (in relative measure).  A cost [<= 0.] means one is satisfied
      with drawing straight lines connecting the three points. *)

  val xy : ?tlog:bool -> ?n:int -> ?strategy:strategy -> ?cost:cost ->
    (float -> float * float) -> float -> float -> float array * float array
  (** [xy f t1 t2] samples the parametric function [f] on the
      interval going from [t1] to [t2].  Returns a list of the points in
      the sample.

      @param tlog do we need to step in a logarithmic way ?

      @param min_step don't increment precision more than this threshold

      @param n is a maximum number of evaluations of [f] that are allowed.
      Default: [100].
      @param strategy a customized strategy.
      @param cost a customized cost.
  *)

  val x : ?tlog:bool -> ?n:int -> ?strategy:strategy -> ?cost:cost ->
    (float -> float) -> float -> float -> float array * float array
  (** [x f x1 x2] same as {!Sampler.xy} but for the scalar function [f]
      on the interval going from [x1] to [x2]. *)

  val strategy_midpoint : strategy
  (** The default strategy: choose the middle point *)

  val strategy_random : strategy
  (** A strategy that avoids aliasing sampling, but not efficient:
      chooses randomly a point between t1 and t2 *)

  val strategy_center_random : strategy
  (** A more efficient strategy that avoids aliasing sampling: chooses
      randomly a points between t1 and t2 in its 10% center interval *)


  val cost_angle : cost
  (** Measures the angle at the middle point [(xm, ym)], the flatter the
      angle, the lower the cost. *)

  val cost_angle_dist : cost
  (** Measures the angle at the middle point [(xm, ym)], the flatter the
      angle, the better and combines it with the distance of the points
      [(x0, y0)] and [(x1, y1)] to the middle point, the smaller the
      distance, the better. *)

  val cost_angle_log : bool -> bool -> cost
  (** Same criterion as {!cost_angle} suitable for logarithmic
      cordinates. *)

end
(*----------------------------------------------------------------------*)
(** {2 High level functions} *)


(** Module handling point styles and marks. *)
module Marker :
sig

  exception Error of string
    (**Raised for undefined point styles.*)

  type name = string
  (** Point styles are identified by strings. *)

  val add : name:name -> (Backend.t -> unit) -> Matrix.rectangle -> unit
  (** [add name f extents] adds to the existing point styles, a new
      point style, referenced under the name [name]. This point style
      is made using the function [f]; the extents it takes is given by
      [extents]. The behaviour of adding a new point style whose name
      is already used by another is the same as the core [Map.S.add]
      (that is, the previous binding disappears).*)

  val names : unit -> name list
  (** @return a list of all names currently declared.

      By default, the following marks are defined (a short explanation
      is given if the mark is not clear from the string):
      - ["x"], ["-"], ["|"], ["+"], ["*"],
      - ["o"] (a circle), ["O"] (a disk, i.e. same as ["o"] but filled),
      - ["s"] (a square), ["S"] (a filled square),
      - ["d"] (a diamond), ["D"] (a filled diamond),
      - ["^"] (an inverted V),  ["v"], [">"], ["<"],
      - ["^-"] (a triangle pointing upward), ["v-"], ["|>"], ["<|"],
      - ["^--"] (a filled triangle pointing upward), ["v--"], ["||>"], ["<||"],
      - ["p"] (a pentagon), ["P"] (a filled pentagon),
      - ["h"] (an hexagon), ["H"] (a filled hexagon),
      - ["tic_up"] (a small bar above the current location),
        ["tic_down"], ["tic_left"], ["tic_right"].
  *)

end



(** Arrow styles  *)
module Arrows :
sig

  (** Style of the arrow end. Below are textual representations of
      those endings *)
  type style =
  | Unstyled      (** ------- *)
  | Simple        (** ------> *)
  | Double        (** ----->> *)
  | Triple        (** ---->>> *)
  | Diamond       (** -----<> *)
  | Circle        (** ------O *)
  | Stop          (** ------| *)
  | Custom of (Path.t -> unit) (** It is also possible to give a path in
                                  the Custom style, leading to a
                                  completely customised arrow *)

  val path_line_to : ?size:float -> ?head:style -> ?tail:style ->
    Path.t -> float -> float -> unit
  (** [path_line_to p x y] Adds an arrow to ([x], [y]) into the path
      [p].  See {!line} for explantation on the optional arguments. *)

  val line_direct : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> unit -> unit
  (** [line_direct vp x0 y0 x y ()] draws a line directly on the
      viewport, withtout using an instruction (see {!line} for usage) *)

  val line : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> unit
  (** [line vp x0 y0 x y] Draws a arrowed line on the viewport [vp] from
      ([x0], [y0]) to ([x], [y]) using an instruction (the drawing of
      the line is put on the queue of elements to draw on the viewport)

      @param size the size of the endings, in marks size

      @param head the head ending style

      @param tail the tail ending style *)

  val arc_direct : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> float -> unit -> unit
  (** [arc_direct vp x0 y0 r a1 a2 ()] draws an arc directly on the
      viewport, withtout using an instruction (see {!arc} for usage) *)

  val arc : ?size:float -> ?head:style -> ?tail:style ->
    Viewport.t -> float -> float -> float -> float -> float -> unit
  (** [arc vp x0 y0 r a1 a2] Draws a arrowed arc on the viewport [vp]
      from ([x0], [y0]) with a starting angle [a1], a ending angle [a2] and
      a radius [r]. Note that the starting point ([x0], [y0]) is called the
      tail of the arrow.

      @param size the size of the endings, in marks size

      @param head the head ending style

      @param tail the tail ending style *)
end


(** Tics position and labels. *)
module Tics :
sig


  type labels =
  | No_label (** [No_label] just draw a vertical bar without any text *)
  | Text of (string * float) array (** Not yet implemented *)
  | Number of int (** [Number n] draw number labels using [n] digits *)
  | Expnumber of float (** Not yet implemented *)
  | Expnumber_named of float * string (** Not yet implemented*)
  | Custom of (float -> string)
  (** [Custom f] for labeling with a function [f] *)

  type t =
  | Fixed of labels * float list
  (** Not Yet implemented *)
  | Fixed_norm of labels * float list
  (** Not Yet implemented *)
  | Equidistants of labels * float * float * int
  (** [Archimede.Tics.Equidistants (label, start, step, n)]
      draws [Major Tics] from [start] equidistants of [step] along the axe
      and with [n] [Minor Tics] between each of them and the labels defined
      with [label] *)
  | Auto of labels
  (** [Archimede.Tics.Auto label] draws [Major Tics] to fit the axe and the
      [label] automaticaly*)

end


(** Routines to draw basic axes systems in a 2-dimensional space. One can
    either draw axes separately using add_(x|y)_axis or use a full default
    axes system with box or cross. *)
module Axes :
sig

  (** The axis can be padded using an offset. It is used to control
      where to place the axis (according to the other axis) *)
  type offset =
  | Relative of float
  (** A relative offset is given in the Data coordinate system. So
      you can ensure that the axis is drawn at the other axis'
      origin (offset: Relative 0.) *)
  | Absolute of float
  (** An absolute offset is given in the Graph coordinate system and
      should have a value between 0 and 1. Using this kind of
      offset, one can ensure to always get the same rendering *)

  val x : ?grid:bool ->
    ?major:(string * float) -> ?minor:(string * float) ->
    ?start:Arrows.style -> ?stop:Arrows.style ->
    ?tics:Tics.t -> ?offset:offset -> Viewport.t -> unit
  (** [x vp] adds an x-axis to the viewport [vp].

      @param major is a couple [(mark, size)] drawn at each major tic
      position.

      @param start the arrow ending style on the left (x0) (see the
      Arrows module)

      @param stop the arrow ending style on the right (xend) (see the
      Arrows module)

      @param tics the "tics policy" for this axis (see the Tics module)

      @param offset where to place the axis (y-coordinate) *)

  val y : ?grid:bool ->
    ?major:(string * float) -> ?minor:(string * float) ->
    ?start:Arrows.style -> ?stop:Arrows.style ->
    ?tics:Tics.t -> ?offset:offset -> Viewport.t -> unit
  (** [y vp] adds an y-axis to the viewport [vp].

      @param start the arrow ending style on the bottom (y0) (see the
      Arrows module)

      @param stop the arrow ending style on the top (yend) (see the
      Arrows module)

      @param tics the "tics policy" for this axis (see the Tics module)

      @param offset where to place the axis (x-coordinate) *)

  val box : ?grid:bool -> ?tics:Tics.t -> ?tics_alt:Tics.t -> Viewport.t -> unit
  (** [box vp] A default system of axes consisting of four axes, one on
      each border of the viewport [vp], resulting in a box surrounding
      the viewport.

      @param tics the "tics policy" for the left and bottom axes (see
      the Tics module for more information over tics policies)

      @param tics_alt the "tics policy" for the right and top axes (see
      the Tics module for more information over tics policies) *)

  val cross : ?tics:Tics.t -> Viewport.t -> unit
  (** [cross vp] A default axes system consisting of two axes, centered
      on the origin ((0, 0) in Data coordinates).

      @param tics the "tics policy" of the axes (see the Tics module for
      more information over tics policies) *)
end

(** {3 Initializing Archimedes} *)

val init : ?lines:float -> ?text:float -> ?marks:float ->
  ?bg:Color.t ->
  ?w:float -> ?h:float -> ?dirs:string list -> string list -> Viewport.t
(** [init backend] initializes Archimedes and returns the main viewport
    using the backend specified.  The first element of [backend] is
    the name (case insensitive) of the underlying engine.  It may be
    followed by one or several options.  For example, ["Graphics"] for
    the graphics backend or ["Cairo"; "PNG"; filename] for the Cairo
    backend, using a PNG surface to be saved to [filename].  The empty
    list selects ["Graphics"; "hold"].

    @param w the width of the main viewport (in backend's unit).

    @param h the height of the main viewport (in backend's unit).

    @param bg the color of the background.  Default: {!Color.white}.

    @param lines the width of the lines.  Default: [1.] which
    corresponds to a line width on the backend of [min w h /. 500.].

    @param text the size of the text.  Default: [12.] which
    corresponds to puting about 42 lines of text in [min w h] height.

    @param marks the size of the marks.  Default: [7.] which
    corresponds packing about 100 marks in [min w h].

    @param dirs a list of directories where Archimedes looks for
    libraries (cma or cmxs) for dynamically loaded backends.  The
    default is the directory where the backends that come with
    Archimedes were installed.
*)

val backend_of_filename : string -> string list
(** Selects a backend according to the filename suffix.  If the suffix
    is not matched (this in particular for [""]), the graphics backend
    is selected. *)

val show : Viewport.t -> unit
(** Alias for {!Viewport.show}. *)

val close : Viewport.t -> unit

val set_color : Viewport.t -> Color.t -> unit
(** Alias for {!Viewport.set_color}. *)

val set_line_width : Viewport.t -> float -> unit
(** Alias for {!Viewport.set_line_width}. *)

val xrange : Viewport.t -> float -> float -> unit
(** Alias for {!Viewport.xrange}. *)

val yrange : Viewport.t -> float -> float -> unit
(** Alias for {!Viewport.yrange}. *)

(** {3 Plotting various datatypes} *)

(** Style of various plots.  Plotting functions only support the
    subset of these style that make sense for them.

    - [`Lines] Data points are joined by a simple line.
    - [`Markers] Data points are marked with the mark type given in
    argument of the Points constructor.
    - [`Linesmarkers] Data points are joined by a line and marked with
    the mark type given in argument.
    - [`Impulses] Data points are "hit" by lines starting from zero.
    - [`Bars w] Data points determine the height of a box of width [w]
    which must be given in [Data] coordinates (from 0 to 1).
    - [`HBars h] Data points determine the width of an horizontal box
    of height [h] which must be given in [Data] coordinates (from 0 to 1).

    For the list of default marks for [`Markers] and [`Linesmarkers]
    have a look to {!Marker.names}.  You can also define your own
    with {!Marker.add}.
*)
type style =
[ `Lines
| `Markers of string
| `Linesmarkers of string
| `Impulses
| `Bars of float
| `HBars of float ]

(** Plotting functions. *)
val fx : Viewport.t -> ?tlog:bool -> ?n:int ->
  ?strategy:Sampler.strategy -> ?cost:Sampler.cost ->
  ?style:[`Lines | `Linesmarkers of string | `Markers of string ] ->
  ?base:(float -> float) -> ?fill:bool -> ?fillcolor:Color.t ->
  (float -> float) -> float -> float -> unit
(** [fx vp f a b] draws the graph of the function [f] on the interval
    [[a, b]].

    @param style the style of the plot.  Default: [`Lines].
    @param fill whether to fill the region between the graph of [f]
    and the base.  Default: [false].
    @param fillcolor the color for filling.  Default: {!Color.white_smoke}.
    @param base the second function for delimiting the filling
    region.  Default: the identically zero function.

    @param n the maximum number of function evaluations.  Default: [100].
    @param strategy see {!Sampler.strategy}.
    @param cost see {!Sampler.cost}. *)

val xyf : Viewport.t -> ?tlog:bool -> ?n:int ->
  ?strategy:Sampler.strategy -> ?cost:Sampler.cost ->
  ?style:[`Lines | `Linesmarkers of string | `Markers of string ] ->
  ?fill:bool -> ?fillcolor:Color.t ->
  (float -> float * float) -> float -> float -> unit
(** [xyf vp f a b] draws the image of the function [f] on the interval
    [[a, b]], that is the set of points (x,y) = [f](t) for t in [[a,b]].

    The optional arguments are the same as for {!fx}. *)


(** Plotting float Arrays. *)
module Array : sig
  val y : Viewport.t -> ?const_base:bool -> ?base:float array ->
    ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_y:bool -> float array -> unit
  (** [y vp yvec] draws the set of points [(i, yvec.(i))].

      @param style the style used for the plot.  The default style is
      [`Marker "O"] which means data points are marked by a small disk.
      See {!Archimedes.style} for a full list.

      @param fill whether to fill the surface between the base and the
      values [yval].
      @param fillcolor the filling color (default: {!Color.white_smoke}).
      @param const_y whether the input vector [yvec] will not be modified
      anymore (so there is no need to cache its current values).

      @param base for the styles [`Lines], [`Markers], and
      [`Linesmarkers], it gives the bottom of the filling zone.  For
      the styles [`Impulses] and [`Bars w], it is the Y value above
      which the boxes (of heights given by [yvec]) are drawn.  For the
      style [`HBars], it is the (signed) distance to the Y axis at
      which the horizontal bar starts.
      @param const_base same as [const_y] for the base. *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_x:bool -> float array -> ?const_y:bool -> float array -> unit
  (** [xy cp xvec yvec] draws the set of points [(xvec.(i), yvec.(i))].
      The optional arguments are similar to {!Array.y}.

      @raise Invalid_argument if [xvec] and [yvec] do not have the same
      length.

      See {!Array.y} for the meaning of optional arguments. *)

  val xy_pairs: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
    (float * float) array -> unit
  (** See {!Array.xy}.  The only difference is that this function
      takes an array of couples (x,y) instead of two arrays, one for x
      and a second of y. *)

  val stack : Viewport.t ->
    ?fill:bool -> ?fillcolors:Color.t array -> ?style:style ->
    ?const:bool -> float array array -> unit
  (** [stack yvecs] plot the data in a stacked fashion, the Y values
      contained in [yvecs.(i)] are represented as the deviation above
      [yvecs.(i-1)].  This makes sense only if the data is non-negative.

      @param style how to represent each data point.  Default [`Bars 0.5].

      @param colors the colors for the data lines.

      @param fill whether to fill the boxes or area under the data
      points.  Default: [true].

      @param fillcolors the [i]th color is used to fill the area under
      the data points [yvecs.(i)].  If the array is empty, a default
      palette is used.  If there are less colors than vectors in
      [yvecs], they are used in a circular way.

      @param const_y whether the input vector [yvec] will not be modified
      anymore (so there is no need to cache its current values). *)
  ;;
end

(** Plotting Lists of floats. *)
module List : sig
  val y : Viewport.t -> ?base:float list -> ?fill:bool ->
    ?fillcolor:Color.t -> ?style:style -> float list -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
    float list -> float list -> unit
  (** See {!Array.xy}.  The number of elements plotted the the minimum
      of the lengths of the two lists. *)

  val xy_pairs: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
    (float * float) list -> unit
  (** See {!Array.xy_pairs}.  *)
end

(** Plotting Fortran bigarrays. *)
module Vec : sig
  open Bigarray
  type t = (float, float64_elt, fortran_layout) Array1.t

  val y : Viewport.t -> ?const_base:bool -> ?base:t -> ?fill:bool ->
    ?fillcolor:Color.t -> ?style:style ->
    ?const_y:bool -> t -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_x:bool -> t -> ?const_y:bool -> t -> unit
  (** See {!Array.xy}.  *)

  val stack : Viewport.t ->
    ?fill:bool -> ?fillcolors:Color.t array -> ?style:style ->
    ?const:bool -> t array -> unit
  (** See {!Array.stack}.  *)
end

(** Plotting C bigarrays. *)
module CVec : sig
  open Bigarray
  type t = (float, float64_elt, c_layout) Array1.t

  val y : Viewport.t -> ?const_base:bool -> ?base:t -> ?fill:bool ->
    ?fillcolor:Color.t -> ?style:style ->
    ?const_y:bool -> t -> unit
  (** See {!Array.y}.  *)

  val xy: Viewport.t -> ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
    ?const_x:bool -> t -> ?const_y:bool -> t -> unit
  (** See {!Array.xy}.  *)

  val stack : Viewport.t ->
    ?fill:bool -> ?fillcolors:Color.t array -> ?style:style ->
    ?const:bool -> t array -> unit
  (** See {!Array.stack}.  *)
end


(*----------------------------------------------------------------------*)
(** {3 Plotting generic data} *)

val y : Viewport.t -> ?base:((float -> unit) -> unit) ->
  ?fill:bool -> ?fillcolor:Color.t -> ?style:style ->
  ((float -> unit) -> unit) -> unit
(** [y vp iter] draws on [vp] the values provided by the iterator [iter].
    See {!Array.y} for more information. *)

val xy : Viewport.t -> ?fill:bool -> ?fillcolor:Color.t ->
    ?style:[`Lines | `Markers of string | `Linesmarkers of string ] ->
  ((float -> float -> unit) -> unit) -> unit
(** [xy vp iter] plots on [vp] the values provided by the iterator
    [iter].
    See {!Array.xy} for more information. *)
(*----------------------------------------------------------------------*)
(** {2 Pie-charts} *)


module Piechart :
sig
  type style =
  | Flat          (** A simple circle separated in regions *)
  | Separated     (** The regions are separated by a gap *)
  | HighlightFlat (** One of the regions is separated by a gap, and so
                      highlighted toward the others *)
  | Relief        (** A simple 3D pie *)

  type colorscheme =
  | Default    (** A set of colors that should render good for any data. *)
  | Monochrome (** A gradient from black to white. *)
  | Black      (** No colors other than black, the [Overpie] or [Outer]
                   keyscheme should then be used. *)
  | CustomColors of (string * Color.t) list
  (** A color scheme associating a custom color to each data. *)
  | ValueDependant of (float -> Color.t)
  (** Sometimes it is desirable to use color to express something on
      the piechart depending on the data values, this color scheme
      permits it. *)
  | LevelValueDependant of (int -> int -> float -> Color.t -> float -> Color.t)
    (** For multi-levels pie charts, the color may depend of the
        level, the position (in the parent children's list), the
        parent data value/color and the actual value of the data to
        draw.  The level 0 is the first level with at least two
        elements. *)

  type keyplacement =
  | Rectangle (** A rectangle containing the color followed by the label
                  of each data *)
  | OverPie   (** The labels are drawn directly over the data *)
  | Outer     (** The labels are drawn around the pie, next to the data they
                  point out *)

  type keylabels =
  | Key          (** Just the name of the data *)
  | WithValues   (** The name followed by the value between parentheses *)
  | WithProcents (** The name followed by the procent among all data between
                     parentheses *)
  | CustomLabels of (string -> float -> float -> string)
  (** A custom label made of the name, the value and the percentage. *)

  val simple : ?style:style -> ?colorscheme:colorscheme ->
    ?keyplacement:keyplacement -> ?keylabels:keylabels ->
    ?x0:float -> ?y0:float -> ?xend:float -> ?yend:float ->
    Viewport.t -> (string * float) list -> unit
  (** [simple vp data] draws a pie chart on [vp].

      @param style the style, default is Relief

      @param colorscheme the color scheme, default is Default

      @param keyplacement where to place the key, default is Rectangle

      @param keylabels what are the labels, default is WithValues

      @param x0 the x-coordinate of the center (default: [0.]).

      @param y0 the y-coordinate of the center (default: [0.]).

      @param xend (default: [1.]).

      @param yend these parameters delimits the area of the pie chart
      over the viewport. They are given in Graph coordinates (i.e. from
      0 to 1). By default, some space is left on the top for a title for
      the pie chart *)

  type multidata = {
    name: string;
    value: float;
    children: multidata list
  }

  val multilevel : ?style:style -> ?colorscheme:colorscheme ->
    ?keyplacement:keyplacement -> ?keylabels:keylabels ->
    ?x0:float -> ?y0:float -> ?xend:float -> ?yend:float ->
    Viewport.t -> multidata list -> unit
  (** [multilevel vp data] draws a multilevel pie chart on [vp]. The
      default options are tuned for a multilevel pie chart

      @param style default is flat (better visualisation because there
      is usualy lots of data)

      @param colorscheme default is LevelValueDependant, colors of the
      first level to contain more than one data (= level 0) are chosen
      in the "Default" way, children colors are derived from their
      parent color and their value. Inner levels (those who contains
      only one data) are filled with blank

      @param keyplacement default is OverPie, this is usually the better
      way to visualize data over a multilevel pie chart

      @param keylabels default is Key, because the color scheme gives an
      idea of the values, it is preferable to save space by hiding the
      values / percentages of the data

      @param x0 the x-coordinate of the center (default: [0.]).

      @param y0 the y-coordinate of the center (default: [0.]).

      @param xend (default: [1.]).

      @param yend by default, space is left for the title, as for the
      simple pie charts *)

end
