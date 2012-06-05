(** Monadic parser interface *)

open Spotlib.Spot

module type S = sig
  module Str : Stream_intf.S
  (** Underlied stream *)

  type error = Str.Pos.t * string
  (** Error is a pair of a source position and a message string. *)

  exception Critical_error of error
  (** Exception of critical error. Used for non recoverable errors. *)

  (** Profiling purpose to count closure constructions. *)
  module Profile : sig
    val incr : unit -> unit
    val format : Format.t -> unit
    val reset : unit -> unit
    val recover_all : unit -> unit (* format will print the over all result *)
  end

  include Monad_intf.T 
  (** Inherit monadic interface. ['a t] is a function, so \eta expandable *)

  (** Parser combinators *)



  (** Retrieve one stream element *)

  val take : Str.elem t
  val take_ : unit t
  (** Take the head token.
      [take_] is the same as [take], but the element is ignored. Following Haskell xxxM_ convention.
  *)

  val token : Str.elem -> unit t
  (** [token e] matches with the token [e] *)
  val tokenp : (Str.elem -> bool) -> Str.elem t
  (** [tokenp p] matches with the token [e] where [p e = true]. *)
  val token_option : (Str.elem -> 'a option) -> 'a t
  (** [token_option f] matches with the token [e] where [f e = Some v] for some [v], and  returns [v]. *)
  val token_result : (Str.elem -> ('a, string) Result.t) -> 'a t
  (** Same as [token_option] but with customizable error messages *)
  val one_of : Str.elem list -> Str.elem t
  (** [one_of tokens] matches with the token [e] where [List.mem e tokens = true] *)



  val tokens : Str.elem list -> unit t
  (** [tokens elems] matches with a sequence of elements [elems]. 
      It is faster than using sequence of token. Tail recursion. 
  *)



  (** Failure recovery *)
    
  val option : 'a t -> 'a option t
  (** [option a] returns [Some v] if [a] succeeds and returns [v]. Otherwise, [option a] returns [None] *)
  val option_ : unit t -> unit t
  (** [option_ a] tries matching [a]. 
      No matter whether [a] succeeds or not, [option_ a] is always successful and returns [()]. *)
  val result : 'a t -> ('a, error) Result.t t
  (** [result a] is as same as [option a] but [error] instead of [None] at error. *)

  val try_finally : 'a t -> (('a, error) Result.t -> unit) -> 'a t
  (** [try_finally t f] performs [t] then run [f] over the result of [t], 
      no matter whether it succeeds or not. The result of the expression is the result of [t].
  *)



  (** Surrounded / listed *)
    
  val surrounded : 'a t -> 'b t -> 'c t -> 'c t
  (** [surrounded left right content] is for [content] prefixed by [left] and postfixed by [right] *)
  (* CR jfuruse: 'a t and 'b t are too general. unit t ? *)
  (* CR jfuruse: labels ? *)

  val list_with_sep : ?optional_head:bool -> sep:unit t -> 'a t -> 'a list t
  (** [list_with_sep ?optional_head ~sep a] is for parsing list of [a] separated by [sep].
      If [optional_head] is true, the list can have optional head separator like ``| A | B | C''.
      Otherwise, the separator must strictly occur between elements, like ``A | B | C''. *) 



  (** Choice and backtrack *)

  val ( <|> ) : 'a t -> 'a t -> 'a t
  (** Parsec's <|>. 
      [a <|> b] first tries [a]. If [a] succeeds, returns it results as the result of the whole expression.
      If [a] fails WITHOUT consuming any input, [b] is tried. 
      If [a] fails using comsuming some input, its error is used as the result of the whole.
  *)

  val try_ : 'a t -> 'a t
  (** Parsec's try. Any failure of [try_ a] is reported as if it did not consume any input. *)
  (** CR jfuruse: _ is used as convention for unit monad, but here, used for avoid name crash... *)

  (** With backtracks *)
  val ( <!> ) : 'a t -> 'a t -> 'a t
  (** Backtrack, unlike [<|>]. (try_ a <|> b) == (a <!> b)

      [a <!> b] first tries [a]. If [a] succeeds, returns it results as the result of the whole expression.
      If [a] fails, no matter whether it consuming any input or not, [b] is tried. 

      Be careful. Careless uses of [<!>] slow down parser efficiency, a lot.
      If you have to use it, think about memoization.
  *)

  val ( <&> ) : 'a t -> ('a -> 'b t) -> 'b t
  (** For push back. [t <&> f] is like [t >>= f], but the result monad's stream position
      is the same as [t]. 

      [a <&> f] first tries [a]. If [a] fails, the entire expression fails.
      If [a] succeeds, [f] is run over the same input [a] takes.
  *)



  (** `*' and `+' operators of Regexp

      NOTE: It is not Regexp. It has no backtrack:
      [?* (token A) >>= fun () -> token A] does not match AAA, since [?* (token A)] eats all the AAA,
      and nothing is left for the latter [token A].
  *)
  val ( ?** ) : 'a t -> 'a list t
  val ( ?* )  : 'a t -> unit t
  (** [?** a] matches with zero or more sequence of [a]. *)

  val ( ?++ ) : 'a t -> 'a list t
  val ( ?+ )  : 'a t -> unit t
  (** [?++ a] matches with one or more sequence of [a]. *)



  (** Regex style operators with backtrack 

      Warn: it performs backtrack and could be very slow. 
      Warn: not tested well at all.
  *)
  val ( /**/ ) : 'a list t -> 'a list t -> 'a list t (** not tail rec *)
  val ( /*/ ) : unit t -> unit t -> unit t
  val ( /++/ ) : 'a list t -> 'a list t -> 'a list t (** not tail rec *)
  val ( /+/ ) : unit t -> unit t -> unit t
  val ( /?/ ) : 'a list t -> 'a list t -> 'a list t



  (** Errors *)
    
  val error : string -> 'a t
  (** [error mes] fails with the error message [mes].
      The error can be caught by other combinators like [<|>] and [<!>].
  *)

  val throw : error -> 'a t
  (** same as [error] but with position *)

  val critical_error : Str.Pos.t -> string -> 'a t
  (** Raise the critical error exception. The exception must be caught by [try ... with].

      Note: The exception is raised when the monad is executed.
      Therefore, it cannot be caught at the monad construction time. It must be caught at the execution.
  *)

  val critical : 'a t -> 'a t
  (** If [t] fails, [critical t] reports its error as a critical error exception *)



  (** Error messages and positions *)

  val ( <?!> ) : 'a t -> string -> 'a t
  (** Enrich error: [m <?!> mes] returns an error message [mes] at error *)

  val ( <?> ) : 'a t -> string -> 'a t
  (** Enrich error: [m <?> mes] returns an error message ["expected " ^ mes] at error *)

  val ( <?@> ) : 'a t -> Str.Pos.t -> 'a t
  (** [m <?@> pos] changes the position of error of [m] to [pos], if happens *)



  (** Streams *)

  val stream : Str.t t
  (** return the underlying stream of the current position *)

  val set_stream : Str.t -> unit t
  (** change the underlying stream *)

  val position : Str.Pos.t t
  (** Returns the position of the head token in the stream. *)



  (** End of stream *)

  val eos : unit t
  (** Matches with End Of Stream *)
  val eos_as_none : 'a t -> 'a option t
  (** End of stream is reported as None *)



  (** Misc *)
    
  val begin_with : 'a t -> 'b t -> 'b option t
  (** [begin_with b w]: if [b] matches then try [w] and returns [w]'s result.
      Otherwise, it returns [None]. *)
  (* CR jfuruse: should be removed *)


  (** Monad runner *)

  val run : 'a t -> Str.t -> ('a * Str.t, error) Result.t
  (** Run the monad over the stream *)
end
