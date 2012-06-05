open Planck
open Spotlib.Spot

module Stream = Token.Stream

type t = Location.t = { loc_start: Lexing.position; loc_end: Lexing.position; loc_ghost: bool }

let to_lexing_position (pos : Position.File.t) : Lexing.position =
  { Lexing.pos_fname = pos.Position.File.fname;
    pos_lnum = pos.Position.File.line;
    pos_bol  = pos.Position.File.byte - pos.Position.File.column;
    pos_cnum = pos.Position.File.byte }

let to_location loc_ghost (pos : Position.Region.t) =
  { Location.loc_start = to_lexing_position pos.Position.Region.start;
    loc_end = to_lexing_position pos.Position.Region.end_;
    loc_ghost = loc_ghost }

(* pos must skip the dummy location of empty match! *)
let symbol_rloc_ref = ref Position.Region.none

let symbol_loc ghost = to_location ghost !symbol_rloc_ref

let symbol_rloc () = symbol_loc false
let symbol_gloc () = symbol_loc true


(* dirty workaround for position *)
let rhs_tbl_stack : (int ref * (int, Position.Region.t) Hashtbl.t * int) list ref = ref []
let rhs_last_pos = ref Position.Region.none

let cntr = ref 0

let push_rhs_tbl name = 
  incr cntr;
  (* Format.eprintf "Push %d (%s)@." !cntr name; *)
  rhs_tbl_stack := (ref 1, Hashtbl.create 13, !cntr) :: !rhs_tbl_stack;
  (name, !cntr)

let pop_rhs_tbl (_name, cntr) = 
  (* Format.eprintf "Popping %d (%s)@." cntr name; *)
  match !rhs_tbl_stack with
  | [] -> assert false
  | (_,_,cntr')::tl -> 
      if cntr = cntr' then rhs_tbl_stack := tl 
      else begin
        (* Format.eprintf "POP MISMATCH %d@." cntr'; *)
        assert false
      end

let set_rhs_counter n = 
  match !rhs_tbl_stack with
  | [] -> assert false
  | (rhs_counter, _, _) :: _ -> rhs_counter := n

let add_rhs_pos pos = 
  match !rhs_tbl_stack with
  | [] -> assert false
  | (rhs_counter, rhs_tbl, _) :: _ -> 
      Hashtbl.replace rhs_tbl !rhs_counter pos;
      incr rhs_counter;
      rhs_last_pos := pos

let rhs_reg n = 
  match !rhs_tbl_stack with
  | [] -> assert false
  | (_rhs_counter, rhs_tbl, _) :: _ ->
      try Hashtbl.find rhs_tbl n with Not_found -> assert false

let rhs_loc n = to_location false (rhs_reg n)

let rhs_replay_with_override (rhs_counter, rhs_tbl, _) pos_regs =
  for i = 1 to !rhs_counter -1 do
    let reg = 
      try List.assoc i pos_regs with Not_found -> Hashtbl.find rhs_tbl i
    in
    add_rhs_pos reg
  done

let prepare_symbol_rloc () =
  match !rhs_tbl_stack with
  | [] -> assert false
  | (rhs_counter, rhs_tbl, _) :: _ ->
(* Now positions cannot be merged...
      let rec merge n reg =
        if n = !rhs_counter then reg
        else
          let reg_n = try Hashtbl.find rhs_tbl n with Not_found -> Position.Region.none in
          (* skip empty region, which is created from rules which consumed nothing. *)
          if reg_n.Position.Region.start = reg_n.Position.Region.end_ then merge (n+1) reg 
          else
            merge (n+1) (Position.Region.merge reg reg_n)
      in
      let reg = merge 1 Position.Region.none in
      (* It is possible when a case consumes no token.
         In that case, we create an empty region from the last pos *)
      let reg = 
        if reg = Position.Region.none then
          { !rhs_last_pos with Position.Region.start = !rhs_last_pos.Position.Region.end_ }
        else reg
      in
*)

      let reg = 
        if !rhs_counter = 1 then { !rhs_last_pos with Position.Region.start = !rhs_last_pos.Position.Region.end_ }
        else 
          let reg_start = Hashtbl.find rhs_tbl 1 in
          let reg_end = Hashtbl.find rhs_tbl (!rhs_counter - 1) in
          { reg_start with Position.Region.end_ = reg_end.Position.Region.end_ }
      in
      symbol_rloc_ref := reg

let calc_rhs_loc last_pos poses =
(* Now positions cannot be merged...
  let reg = 
    List.fold_left Position.Region.merge Position.Region.none poses
  in
  if reg = Position.Region.none then 
    { last_pos with Position.Region.start = last_pos.Position.Region.end_ } 
  else reg
*)
  match poses with
  | [] -> { last_pos with Position.Region.start = last_pos.Position.Region.end_ } 
  | [reg] -> reg
  | reg_start::regs ->
      let reg_end = List.hd (List.rev regs) in
      { reg_start with Position.Region.end_ = reg_end.Position.Region.end_ }

let calc_symbol_rloc last_pos poses =
  calc_rhs_loc last_pos (List.filter (fun reg -> reg.Position.Region.start <> reg.Position.Region.end_) poses)
