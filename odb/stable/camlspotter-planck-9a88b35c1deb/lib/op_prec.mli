(** Tools for operator precedences and associativities *)

type 'a t
(** This is the abstract type of ``unresolved parse tree''.

    Type [s t] carries parse tree [s] whose operator precedences + associativities
    are not fully resolved yet.

    At parsing an expression with operators of type [s], 
    first you can parse its sub-expressions from left to right 
    without thinking about operator precedences or associativities. 
    Instead, build [s t] instead of [s] using the combinators below.
    Once the whole expression with operators is parsed as [s t], then
    obtain the parse tree [s] by [build], which resolves all the necessary
    operator precedences and associativities.

    You can see a use example of this module at planck/test/expr.ml
*)

type op = {
  prec : float; (** Operator precedence. Bigger is stronger. *)
  kind : [ `Infix of [ `Left | `Right ] (** Infix, left or right associative *)
         | `Noassoc 
         | `Postfix 
         | `Prefix
         ];
}

val tbl : (string, op) Hashtbl.t
(** Operator table. The resolution functions below uses this table to query operators
    and its precedence information *)

val find : (string -> op) ref
(** By default [!find] is used to query operators from [tbl]. 
    If your set of operators cannot use table based precedence + associativity query,
    you can override this behavior by modifying the reference. 
*)

val terminal : 'a -> 'a t
(** [terminal a] takes a terminal object and returns its unresolved tree.
    Terminal objects are leaves in parse trees which cannot be affected by operators: 
    ex. ``10'', ``"hello"'', ``var'' 
*)
  
val parened : ('a -> 'a) -> 'a t -> 'a t
(** [parened f at] creates the unresolved parse tree for the parenthesized expression
    which [at] represents. For example, if [at] represents ``1 + 2'', [parened f at]
    represents ``(1 + 2)''. At resolution, the result of [at] is given to the funciton 
    [f] to create the result of [parened f at].
*)

val infix : string -> ('a -> 'a -> 'a) -> 'a t -> 'a t -> 'a t
(** [infix name f at1 at2] creates the unresolved parse tree for the expression
    of a binary operator use [name] whose left argument is [at1] and right is [at2].
    At resolution, the precedence and associativity of [name] is obtained from [find]
    function above. Function [f] is used to create the result of the resolution. *)

val prefix : string -> ('a -> 'a) -> 'a t -> 'a t
(** [prefix name f at] creates the unresolved parse tree for the expression
    prefixed by an operator [name] whose argument is [at].
    At resolution, the precedence and associativity of [name] is obtained from [find]
    function above. Function [f] is used to create the result of the resolution. *)

val postfix : string -> ('a -> 'a) -> 'a t -> 'a t
(** [postfix name f at] is just like [prefix name f at], but for postfix operators. *)

val list : string -> ('a list -> 'a) -> 'a t list -> 'a t
(** [list name f ats] creates the unresolved parse tree for the list style expression
    which is separated by the operator [name] and has the elements [ats]. *)

val build : 'a t -> 'a
(** [build at] analyzes all the required precedencies + associativities in 
    the unresolved parse tree [at] and returns the resolution result. *)
