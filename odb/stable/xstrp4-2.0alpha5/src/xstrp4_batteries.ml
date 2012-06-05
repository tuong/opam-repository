open Camlp4.PreCast
open Syntax

module I : Xstrp4.Implementation_sig = struct
  (** Use the default implementation as a base *)
  include Xstrp4.Default_implementation

  (** [translate_custom_variable ~v ~f _loc] acts like
      {!Xstrp4.Default.translate_custom_variable} except that it uses
      {!BatPrintf.sprintf2} instead of {!Printf.sprintf}. *)
  let translate_custom_variable ~v ~f _loc _loc_f =
    let v = Gram.parse_string expr _loc v in
    let f = Gram.parse_string expr _loc_f f in
    (* For some reason, the reported location is wrong here *)
    <:expr< BatPrintf.sprintf2 "%a" $f$ $v$ >>
end

module M = Xstrp4.Make(I)(Xstrp4.Default_lexer)

