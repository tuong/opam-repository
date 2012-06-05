open Sexplib.Conv

type op = { 
  prec : float; (* just a joke :-) *)
  kind : [ `Infix of [ `Left | `Right ] | `Prefix | `Postfix | `Noassoc ]
} with sexp

let tbl : (string, op) Hashtbl.t = Hashtbl.create 107

(* List like e1, e2, ..., en is implemented as infix with special builder *)
type 'v t = 
  | Infix of op * [ `List of 'v list -> 'v | `Binop of ('v -> 'v -> 'v) ] * 'v t * 'v t
  | Parened of ('v -> 'v) * 'v t
  | Postfix of op * ('v -> 'v) * 'v t
  | Prefix of op * ('v -> 'v) * 'v t
  | Terminal of 'v
with sexp

let compare x y =
  match compare x.prec y.prec with
  | 1 -> `Strong
  | -1 -> `Weak
  | 0 ->
      if x.kind = y.kind then 
        match x.kind with
        | `Infix `Left -> `Left
        | `Infix `Right -> `Right
        | `Prefix -> assert false
        | `Postfix -> assert false
        | `Noassoc -> `Error
      else `Error
  | _ -> assert false

let rec build = function
  | Terminal e -> e
  | Parened (f, e) -> f (build e)
  | Prefix (_op, f, e) | Postfix (_op, f, e) -> f (build e)
  | Infix (_op, `Binop f, left, right) -> f (build left) (build right)
  | Infix (op, `List f, left, right) -> f (List.map build (build_list op left right))
      
(* stick list elements together *)
and build_list op e1 e2 =
  let e1s = 
    match e1 with
    | Infix (op', _, e11, e12) when op = op' -> build_list op e11 e12
    | _ -> [e1]
  in
  let e2s =
    match e2 with
    | Infix (op', _, e21, e22) when op = op' -> build_list op e21 e22
    | _ -> [e2]
  in
  e1s @ e2s

let pp ppf v = Sexplib.Sexp.pp_hum ppf (sexp_of_t (fun _ -> Sexplib.Sexp.Atom "") v)

(*
let build v = 
  Format.eprintf "BUILD: %a@." pp v;
  let res = build v in
  Format.eprintf "BUILD DONE@.";
  res
*)

let terminal x = Terminal x

let parened f e = Parened (f, e)

let rec infix op f left right = infix_left op f left right 

and infix_left op f e12 e3 =
  match e12 with
  | Parened _ | Terminal _ -> infix_right op f e12 e3
  | Prefix (lop, lf, e) -> 
      begin match compare op lop with
      | `Strong -> prefix lop lf (infix op f e e3)
      | `Weak -> infix_right op f e12 e3
      | `Error (* CR jfuruse: ? *) -> infix_right op f e12 e3 (* - 1 + _ => <- 1> + _ *)
      | `Left | `Right -> assert false
      end
  | Postfix _ ->
      (* always a! * b => <a!> * b, when ! is postfix *)
      infix_right op f e12 e3
  | Infix (lop, lf, e1, e2) -> 
      begin match compare op lop with
      | `Strong | `Right -> infix lop lf e1 (infix op f e2 e3)
      | `Weak | `Left -> infix_right op f e12 e3
      | `Error -> assert false
      end

and infix_right op f e1 e23 =
  match e23 with
  | Parened _ | Terminal _ -> Infix (op, f, e1, e23)
  | Prefix _ -> 
      (* always a * - b => a * <- b>, when - is prefix *) 
      Infix (op, f, e1, e23)
  | Postfix (rop, rf, e) -> 
      begin match compare op rop with
      | `Strong -> postfix rop rf (infix op f e1 e)
      | `Weak -> Infix (op, f, e1, e23) 
      | `Error (* CR jfuruse: ? *) -> Infix (op, f, e1, e23) (* _ + 1 x => _ + <1 x> *)
      | `Left | `Right -> assert false
      end
  | Infix (rop, rf, e2, e3) ->
      match compare op rop with
      | `Strong | `Left -> infix rop rf (infix op f e1 e2) e3
      | `Weak | `Right -> Infix (op, f, e1, e23)
      | `Error -> assert false

and prefix op f e =
  match e with
  | Parened _ | Terminal _ | Prefix _ (* left - left *) -> Prefix (op, f, e)
  | Postfix (op', f', e') -> (* - e ! *)
      begin match compare op op' with
      | `Strong -> postfix op' f' (prefix op f e')
      | `Weak -> Prefix (op, f, e)
      | _ -> assert false
      end
  | Infix (rop, rf, e1, e2) ->
      begin match compare op rop with
      | `Weak -> Prefix (op, f, e)
      | _ -> infix rop rf (prefix op f e1) e2 (* - <1 + 2> => (-1) + 2 *) 
      end

and postfix op f e =
  match e with
  | Parened _ | Terminal _ | Postfix _ (* right - right *) -> Postfix (op, f, e)
  | Prefix (op', f', e') -> (* - e ! *)
      begin match compare op op' with
      | `Strong -> prefix op' f' (postfix op f e')
      | `Weak -> Postfix (op, f, e)
      | _ -> assert false
      end
  | Infix (rop, rf, e1, e2) ->
      match compare op rop with
      | `Weak -> Postfix (op, f, e)
      | _ -> infix rop rf e1 (postfix op f e2)

      
  

  
(*
let list op f = function
  | [] -> []
  | [e] -> [e]
  | e1::e2::es ->
*)      
      
  

(*
let infix op f left right = 
  Format.eprintf "INFIX: %a (%a) (%a)@."
    Sexplib.Sexp.pp_hum (sexp_of_op op)
    pp left
    pp right;
  let res = infix op f left right in
  Format.eprintf "INFIX DONE@.";
  res

let prefix op f e = 
  Format.eprintf "PREFIX: %a (%a)@."
    Sexplib.Sexp.pp_hum (sexp_of_op op)
    pp e;
  let res = prefix op f e in
  Format.eprintf "PREFIX DONE@.";
  res

let postfix op f e = 
  Format.eprintf "POSTFIX: %a (%a)@."
    Sexplib.Sexp.pp_hum (sexp_of_op op)
    pp e;
  let res = postfix op f e in
  Format.eprintf "POSTFIX DONE@.";
  res
*)

exception Op_not_found of string
let find_tbl op = try Hashtbl.find tbl op with Not_found -> raise (Op_not_found op)
let find = ref (fun op -> find_tbl op)
let list op f = function
  | [] -> assert false
  | e::es -> 
      let op = !find op in
      List.fold_left (infix op (`List f)) e es
let infix op f = infix (!find op) (`Binop f)
let prefix op = prefix (!find op)
let postfix op = postfix (!find op)
