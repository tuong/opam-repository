open Camlp4.PreCast
open Syntax

module Types = Xstrp4_types

open Types

let camlp4loc (loc1,loc2) =
  Loc.merge
    (Loc.of_lexing_position loc1)
    (Loc.of_lexing_position loc2)

(** Lexer signature required for an xstrp4 implementation *)
module type Lexer_sig = sig
  val token : Lexing.lexbuf -> Xstrp4_types.clause_t
end

(** Any xstrp4 implementation will need to match this module signature.  Each
    function should return an expression which evaluates to a string.  This
    string will be the interpolated result of the ['$'] interpolation. *)
module type Implementation_sig = sig
  (** [translate_literal ~v _loc] should return an expression to put in place
      of a literal [$foo] or [$(foo)] interpolated value. *)
  val translate_literal : v:string -> Loc.t -> Ast.expr

  (** [translate_variable ~v ~fmt _v_loc _fmt_loc] should return an expression
      to put in place of a variable + printf-like format, from the form
      [${v, %fmt}].  [fmt] will include the leading ['%']. *)
  val translate_variable : v:string -> fmt:string -> Loc.t -> Loc.t -> Ast.expr

  (** [translate_custom_variable ~v ~f _v_loc _f_loc] should return an
      expression to put in place of a variable + string conversion function,
      from the format [${v, f}]. *)
  val translate_custom_variable : v:string -> f:string -> Loc.t -> Loc.t -> Ast.expr
end

(** This module implements the base [Xstrp4_here] version of this extension.
    It can be included as a base for creating custom xstrp4 extensions. *)
module Default_implementation : Implementation_sig = struct
  (** A string value will be interpolated as-is. *)
  let translate_literal ~v _loc =
    <:expr<$str:v$>>

  (** A variable + format will be interpolated using {!Printf.sprintf}. *)
  let translate_variable ~v ~fmt _loc _loc_fmt =
    let fmt_expr = <:expr@_loc_fmt< $str:fmt$ >> in
    let v = Gram.parse_string expr _loc v in
    match fmt with
    | "%s" ->
        <:expr< $v$ >>
    | ("%d"|"%i") ->
        <:expr< Pervasives.string_of_int $v$ >>
    | _ ->
        <:expr< Printf.sprintf $fmt_expr$ $v$ >>

  (** A custom variable will create an interpolation result using [f v]. *)
  let translate_custom_variable ~v ~f _loc _loc_f =
    let v = Gram.parse_string expr _loc v in
    let f = Gram.parse_string expr _loc_f ("fun () -> " ^ f) in
    (* For some reason, the reported location is wrong here *)
    <:expr< Printf.sprintf "%a" $f$ $v$ >>
end

(** This module implements the lexer for the base [Xstrp4_here] version of this
    extension. *)
module Default_lexer = Xstrp4_lexer

(** A functor to create a customized xstrp4 interpolation module, based on a
    given implementation and lexer. *)
module Make(I : Implementation_sig)(L : Lexer_sig) = struct
  class camlp4reloc reloc =
  object
    inherit Ast.map

    method loc _ = reloc
  end

  (** [interpolated_expr lexbuf _loc] returns the interpolated form of
      [lexbuf]. *)
  let interpolated_expr lexbuf _loc =
    (* Parse [lexbuf], and generate the syntax tree for the corresponding expression.
     *)

    let rec parse_expr () =
      let tok = L.token lexbuf in
      match tok with
      | Textend -> []
      | x       -> x :: parse_expr ()
    in

    let rec normalize_literals =
      (* - Concat adjacent literals
       * - Remove empty literals
       *)
      function
      | [] ->
          []
      | Literal("",_) :: tl ->
          normalize_literals tl
      | Literal(s1,(p1,_)) :: (Literal(s2,(_,p2))) :: tl ->
          normalize_literals((Literal(s1^s2,(p1,p2)))::tl)
      | hd :: tl ->
          hd :: (normalize_literals tl)
    in

    let fix_position p =
      {
        Lexing.pos_fname = Loc.file_name _loc;
        Lexing.pos_lnum = p.Lexing.pos_lnum + Loc.start_line _loc - 1;
        Lexing.pos_cnum = p.Lexing.pos_cnum + Loc.start_off _loc;
        Lexing.pos_bol  = p.Lexing.pos_bol  + Loc.start_bol _loc;
      }
    in

    let fix_positions = function
      | Literal(s, (p1, p2)) ->
          Literal(s, (fix_position p1, fix_position p2))
      | Variable(sl, (ps1, ps2), fmt, (pf1, pf2)) ->
          Variable(sl, (fix_position ps1, fix_position ps2), fmt, (fix_position pf1, fix_position pf2))
      | Custom_variable (a, (pa1, pa2), b, (pb1, pb2)) ->
          Custom_variable (a, (fix_position pa1, fix_position pa2), b, (fix_position pb1, fix_position pb2))
      | other ->
          other
    in

    let toklist =
      List.map fix_positions (normalize_literals (parse_expr ()))
    in

    (* Use functions from [M] to translate each interpolation *)
    let toklist_ast =
      let translate = function
        | Literal (v, lexloc) ->
            I.translate_literal ~v (camlp4loc lexloc)
        | Variable (v, v_lexloc, fmt, f_lexloc) ->
            I.translate_variable ~v ~fmt (camlp4loc v_lexloc) (camlp4loc f_lexloc)
        | Custom_variable (v, v_lexloc, f, f_lexloc) ->
            I.translate_custom_variable ~v ~f (camlp4loc v_lexloc) (camlp4loc f_lexloc)
        | Textend ->
            failwith "Xstrp4 translate"
      in
      List.map translate toklist
    in

    let rec mk_list_ast l =
      match l with
      | [] -> <:expr@here< [] >>
      | x :: l' ->
          let ast_l' = mk_list_ast l' in
          <:expr@here< [ $x$ :: $ast_l'$ ] >>
    in

    let string_mod_ast =    <:expr@here< $uid:"String"$ >> in
    let concat_val_ast =    <:expr@here< $lid:"concat"$ >> in
    let string_concat_ast = <:expr@here< $string_mod_ast$ . $concat_val_ast$ >> in
    let concat_ast =        <:expr@here< $string_concat_ast$ $str:""$ >> in
    let list_ast =          mk_list_ast toklist_ast in
    let result_ast =        <:expr@here< $concat_ast$ $list_ast$ >> in

    match toklist with
    | [] ->
        <:expr@here< $str:""$ >>
    | [Literal s] ->
        List.hd toklist_ast   (* = <:expr< $str:s$ >> *)
    | _ ->
        (* General case: *)
        result_ast
end

