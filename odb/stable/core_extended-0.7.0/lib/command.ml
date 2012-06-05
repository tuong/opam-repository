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

open Core.Std
open Printf

(* simple module for formatting two columns *)
module Columns = struct
  type t = (string * string) list
  let align list : string list =
    match list with
    | [] -> []
    | (x,_) :: xs ->
      let left_col_len : int =
        List.fold_left
          (List.map ~f:(fun (a,_) -> String.length a) xs)
          ~f:(fun acc x -> max acc x)
          ~init:(String.length x)
      in
      let max_width = Option.value (Console.width ()) ~default:80 in
      List.concat
        (List.map list
           ~f:(fun (cmd,desc) ->
             let desc_line_len = max 30 (max_width - 4 - left_col_len) in
             let desch,descl =
               match Extended_string.line_break desc ~len:desc_line_len with
               | h::t -> h,t
               | [] -> assert false
             in
             let head =
             (sprintf "%-*s  %s"
                left_col_len
                cmd
                desch)
             in
             let tail =
               List.map descl ~f:(fun s ->
                 sprintf "%-*s  %s"
                   left_col_len
                   " "
                   s)
             in
             head :: tail))

  let sort_align list =
    let list = List.sort
      list
      ~cmp:(fun (a,_) (c,_) ->
        if a = "help" then 1 else if c = "help" then -1 else 0)
    in
    align list

end

module Help_page = struct

  (* A special string list sort which sorts the string "help" as the last
     element *)

  let help_last_sort list =
    let rec extract_help ?(acc=[]) list = match list with
      | hd :: tl when String.is_prefix ~prefix:"help" hd ->
          let acc = List.rev_append tl acc in
          let acc = hd :: acc in
          List.rev acc
      | hd :: tl ->
          extract_help tl ~acc:(hd :: acc)
      | [] -> List.rev acc
    in
    extract_help list
  ;;

  let render ~summary ~usage ~choice_type ~choices =
    sprintf "\n%s\n\n  %s\n%s" summary usage
      (if choices = [] then "" else
          sprintf "\n  === %s ===\n\n%s\n" choice_type
            (List.fold choices ~init:""
               ~f:(fun acc x -> acc ^ "  " ^ x ^ "\n")))
  ;;

end

let partial_match tbl subcmd =
  let keys = Hashtbl.keys tbl in
  let ms = List.filter keys ~f:((=)subcmd) in
  match ms with
  | [key] -> Hashtbl.find tbl key
  | _ -> (* No full match, try for a partial match *)
      begin
        let ms = List.filter_map keys ~f:(fun key ->
          if String.is_prefix key ~prefix:subcmd then Some key else None)
        in
        match ms with
        | [key] -> Hashtbl.find tbl key
        | _ -> None (* No unique match *)
      end
;;

let no_underscores s =
  if String.exists s ~f:(fun c -> c = '_')
  then failwithf "%s contains an underscore. Use a dash instead." s ();

module Flag : sig
  (** type of flags to a command with accumulator type ['a] *)
  type 'a t
  val name : _ t -> string

  val help : _ t -> (string * string) list

  (** Template for flag-creation functions *)
  type ('a, 'b) create =
    string
    -> ?aliases:string list
    -> ('b)
    -> doc:string
    -> 'a t

  (** {6 Flag handling without an accumulator} *)

  val noarg      : (unit, unit -> unit)   create
  val string     : (unit, string -> unit) create
  val int        : (unit, int -> unit)    create
  val float      : (unit, float -> unit)  create
  val bool       : (unit, bool -> unit)   create

  val set_string     : (unit, string ref)        create
  val set_string_opt : (unit, string option ref) create
  val set_int        : (unit, int ref)           create
  val set_int_opt    : (unit, int option ref)    create
  val set_float      : (unit, float ref)         create
  val set_float_opt  : (unit, float option ref)  create
  val set            : (unit, bool ref)          create
  val clear          : (unit, bool ref)          create

  (** {6 flag handling meant for use with immutable accumulator} *)

  val noarg_acc  : ('a, 'a -> 'a) create
  val string_acc : ('a, 'a -> string -> 'a) create
  val int_acc    : ('a, 'a -> int -> 'a) create
  val bool_acc   : ('a, 'a -> bool -> 'a) create
  val float_acc  : ('a, 'a -> float -> 'a) create
  (** [rest f]: a flag that signals the end of flag processing.  all remaining arguments
      are passed to the [f] *)
  val rest_acc   : ('a, 'a -> string list -> 'a) create

  (** {6 flag handling meant for use with mutable accumulator} *)

  val noarg_mut  : ('a, 'a -> unit) create
  val arg_mut    : ('a, 'a -> string -> unit) create
  val string_mut : ('a, 'a -> string -> unit) create
  val int_mut    : ('a, 'a -> int -> unit) create
  val bool_mut   : ('a, 'a -> bool -> unit) create
  val float_mut  : ('a, 'a -> float -> unit) create
  (** [rest f]: a flag that signals the end of flag processing.  all remaining arguments
      are passed to the [f] *)
  val rest_mut : ('a, 'a -> string list -> unit) create


  (** {2 Deprecated } This is the old deprecated interface to Flag *)
  module Action : sig
    (** ['accum]-mutating action to perform when processing a flag *)
    type 'accum t =
    | Noarg of ('accum -> 'accum)
    | Arg of ('accum -> string -> 'accum)
    | Rest of ('accum -> string list -> 'accum)

    (** an action for a flag that takes no additional argument *)
    val noarg : ('accum -> unit) -> 'accum t

    (** an action for a flag that takes an additional string argument *)
    val arg : ('accum -> string -> unit) -> 'accum t

    (** [rest f]: an action for a flag that signals the end of flag
        processing.  all remaining arguments are passed to the [f] *)
    val rest : ('accum -> string list -> unit) -> 'accum t
      (** [rest f]: an action for a flag that signals the end of flag
            processing.  all remaining arguments are passed to the [f] *)

    (** an action for a flag that takes an additional int argument *)
    val int : ('accum -> int -> unit) -> 'accum t

    (** an action for a flag that takes an additional bool argument *)
    val bool : ('accum -> bool -> unit) -> 'accum t

    (** an action for a flag that takes an additional float argument *)
    val float : ('accum -> float -> unit) -> 'accum t
  end

  val create :
    ?aliases:string list
    -> name:string
    -> doc:string
    -> 'a Action.t
    -> 'a t

  (** [lift t ~project] transforms a flag with accumulator type ['a]
      into a flag with a more informative accumulator type ['b]
      provided that [project x] returns a pair consisting of
        1. a ['a]-value extracted from the ['b]-value [x], and
        2. a function for rebuilding a modified ['b]-value from
           the modified ['a]-value resulting from processing the flag.

      The intended use pattern for [lift] is when ['b] is a record type
      with a field [foo] of type ['a] and [project] is
        [fun r -> (r.foo, (fun foo' -> { r with foo = foo' }))]
  *)
  val lift : 'a t -> project:('b -> 'a * ('a -> 'b)) -> 'b t

  val lookup : 'a t list -> string -> 'a Action.t option
end = struct

  module Action' = struct
    type 'a t =
      | Noarg of ('a -> 'a)
      | Arg of ('a -> string -> 'a)
      | Rest of ('a -> string list -> 'a)

    let noarg f = Noarg f
    let arg f = Arg f
    let rest f = Rest f

    let int   f = Arg (fun x s -> f x (Int.of_string   s))
    let bool  f = Arg (fun x s -> f x (Bool.of_string  s))
    let float f = Arg (fun x s -> f x (Float.of_string s))

    (* [project] extracts a record field together with a rebuilding
       function [inject] to fill in the remaining fields after modifying
       the field. *)
    let lift t ~project =
      match t with
      | Noarg g ->
        Noarg (fun r -> let (f, inject) = project r in inject (g f))
      | Arg g ->
        Arg (fun r x -> let (f, inject) = project r in inject (g f x))
      | Rest g ->
        Rest (fun r xs -> let (f, inject) = project r in inject (g f xs))
    ;;
  end

  (* Action is a deprecated interface, that we here gussy up to be usable on the
     outside.  Action' is the primary interface here. *)
  module Action = struct
    type 'a t = 'a Action'.t =
      | Noarg of ('a -> 'a)
      | Arg of ('a -> string -> 'a)
      | Rest of ('a -> string list -> 'a)

    let cvt x a =
      a (fun acc v -> x acc v; acc)

    let noarg x = Action'.noarg (fun acc -> x acc; acc)
    let arg   x = cvt x Action'.arg
    let rest  x = cvt x Action'.rest
    let int   x = cvt x Action'.int
    let bool  x = cvt x Action'.bool
    let float x = cvt x Action'.float
  end

  type 'a t = {
    name : string;
    spec : 'a Action.t;
    doc : string;
    aliases : string list;
  }

  let lift t ~project = { t with spec = Action'.lift t.spec ~project }

  let create ?(aliases=[]) ~name ~doc spec =
    no_underscores name;
    {
      name = name;
      spec = spec;
      doc = doc;
      aliases = aliases;
    }

  let name v = v.name

  let lookup ts =
    let alist =
      List.concat_map ts ~f:(fun t ->
        List.map (t.name :: t.aliases) ~f:(fun v ->
          (v, t.spec)))
    in
    match String.Table.of_alist alist with
    | `Duplicate_key flag ->
        failwithf "multiple specifications for flag %s" flag ()
    | `Ok tbl -> (fun flag -> partial_match tbl flag)
  ;;

  let help { name = name; doc = doc; aliases = aliases}  =
    if String.is_prefix doc ~prefix:" " then
      (name, String.lstrip doc) ::
        List.map aliases
        ~f:(fun x -> x,sprintf "same as \"%s\"" name)
    else
      let (arg, doc) =
        match String.lsplit2 doc ~on:' ' with
        | None -> (doc, "")
        | Some pair -> pair
      in
      (name ^ " " ^ arg, String.lstrip doc) ::
        List.map aliases
        ~f:(fun x -> x ^ " " ^ arg,sprintf "same as \"%s\"" name)
  ;;


  (* The creation functions are listed below *)
  type ('a,'b) create =
    string
    -> ?aliases:string list
    -> 'b
    -> doc:string
    -> 'a t

  (* takes a mutable-style handling function, and returns one that has an interface like
     an immutable-style handling function *)
  let unmut handle =
    (fun acc arg -> handle acc arg; acc)
  ;;

  let of_action fix_handle action_fn =
    (fun name ?aliases handle ~doc ->
      create ?aliases ~name ~doc (action_fn (fix_handle handle)))
  ;;

  let noarg_mut x = of_action (fun h x -> h x; x) Action'.noarg x
  let arg_mut    x = of_action unmut Action'.arg   x
  let string_mut x = of_action unmut Action'.arg   x
  let int_mut    x = of_action unmut Action'.int   x
  let bool_mut   x = of_action unmut Action'.bool  x
  let float_mut  x = of_action unmut Action'.float x
  let rest_mut   x = of_action unmut Action'.rest  x

  let noarg_acc  x = of_action Fn.id Action'.noarg x
  let string_acc x = of_action Fn.id Action'.arg   x
  let int_acc    x = of_action Fn.id Action'.int   x
  let bool_acc   x = of_action Fn.id Action'.bool  x
  let float_acc  x = of_action Fn.id Action'.float x
  let rest_acc   x = of_action Fn.id Action'.rest  x

  let unref ref =
    (fun () x -> ref := x)
  let unref_opt ref =
    (fun () x -> ref := Some x)
  let unclos f =
    (fun () x -> f x)


  let set x = of_action (fun ref () -> ref := true) Action.noarg x
  let clear x =  of_action (fun ref () -> ref := false) Action.noarg x

  let noarg          x = of_action Fn.id      Action'.noarg x
  let string         x = of_action unclos     Action'.arg   x
  let int            x = of_action unclos     Action'.int   x
  let bool           x = of_action unclos     Action'.bool  x
  let float          x = of_action unclos     Action'.float x
  let set_string     x = of_action unref      Action'.arg   x
  let set_string_opt x = of_action unref_opt  Action'.arg   x
  let set_int        x = of_action unref      Action'.int   x
  let set_int_opt    x = of_action unref_opt  Action'.int   x
  let set_float      x = of_action unref      Action'.float x
  let set_float_opt  x = of_action unref_opt  Action'.float x

end

module Shared_flags = struct
  type 'a t = {
    flags : unit Flag.t list;
    get : unit -> 'a;
  }
end


let ifs = Option.value ~default:" " (Sys.getenv "IFS")

module Autocomplete_ = struct
  type t = string list -> string list

  module Bash_action = struct
    type t =
      [ `Alias | `Arrayvar | `Binding | `Builtin | `Command | `Directory
        | `Disabled | `Enabled | `Export | `File | `Function | `Group
        | `Helptopic | `Hostname | `Job | `Keyword | `Running | `Service
        | `Setopt | `Shopt | `Signal | `Stopped | `User | `Variable
      ] with sexp

    let to_string action =
      sexp_of_t action |! Sexp.to_string |! String.lowercase
  end

  let escape_for_bash string =
    let len = String.length string in
    let buffer = Buffer.create len in
    for i = 0 to len - 1 do
      begin match string.[i] with
      | '|' | '&' | ';' | '(' | ')' | '<' | '>' | ' ' | '\t' ->
          Buffer.add_char buffer '\\';
      | _ -> ()
      end;
      Buffer.add_char buffer string.[i];
    done;
    Buffer.contents buffer

  let bash =
    let create_command_line action key =
      let options =
        match action with
        | `Directory -> " -S/ "
        | _ -> " "
      in
      "compgen" ^ options ^ " -A " ^ Bash_action.to_string action ^ " " ^ key
    in
    let single_command action key =
      let command_line = create_command_line action key in
      try Shell.run_full "/bin/bash" ["-c"; command_line] with
      | _exn -> ""
    in
    let command actions key =
      let actions =
        if
          List.exists actions ~f:((=) `File)
          && not (List.exists actions ~f:((=) `Directory))
        then
          `Directory :: actions
        else actions
      in
      List.map ~f:(fun action -> single_command action key) actions
      |!  String.concat ~sep:"\n"
    in
    fun actions command_line ->
      let result =
        match List.rev command_line with
        | [] -> command actions ""
        | key :: _ -> command actions key
      in
      String.split ~on:'\n' result |! List.map ~f:escape_for_bash
  ;;
end

let maybe_dashify ~allow_underscores =
  if not allow_underscores
  then (fun s -> s)
  else (fun s ->
    String.mapi s ~f:(fun i c ->
      if i = 0 || c <> '_'
      then c
      else '-'
    ))

module Explicit = struct

  type ('a, 'b) t = {
    summary : string;
    usage_arg : string;
    init : unit -> 'a;
    autocomplete : Autocomplete_.t option;
    flags : 'a Flag.t list;
    final : 'a -> string list -> 'b;
    main : 'b -> unit;
  }

  let summary t = t.summary

  let usage_arg t = t.usage_arg

  let help ~cmd t =
    let helps = List.concat_map ~f:Flag.help t.flags in
    Help_page.render
      ~summary:(summary t)
      ~usage:(cmd ^ " " ^ t.usage_arg)
      ~choice_type:"available flags"
      ~choices:(Columns.sort_align helps)
  ;;

  exception Help

  let run t ~allow_unknown_flags ~allow_underscores
      ~cmd ~argv ~(exit : int -> unit) =
    let maybe_dashify = maybe_dashify ~allow_underscores in
    let rec process_flags ~state ~anon_args args =
      match args with
      | [] -> (state, List.rev anon_args)
      | arg :: argv ->
        if not (String.is_prefix ~prefix:"-" arg) then
          process_flags ~state argv ~anon_args:(arg :: anon_args)
        else
          let flag = maybe_dashify arg in
          if String.equal flag "-help" then raise Help
          else match Flag.lookup t.flags flag with
          | None ->
            if allow_unknown_flags then begin
              process_flags ~state argv ~anon_args:(arg :: anon_args)
            end else failwithf "unknown flag %s" arg ()
          | Some (Flag.Action.Noarg f) -> process_flags ~state:(f state) ~anon_args argv
          | Some (Flag.Action.Rest f) -> (f state argv, anon_args)
          | Some (Flag.Action.Arg f) ->
            match argv with
            | arg :: argv -> process_flags ~state:(f state arg) argv ~anon_args
            | [] -> failwithf "missing argument for flag %s" arg ()
    in
    let process_flags ~state args = process_flags ~state args ~anon_args:[] in
    let (>>=) = Result.(>>=) in
    let (>>|) = Result.(>>|) in
    (* [process_flags] must occur before the closure below is returned.  This
       is an essential aspect of the semantics of [run_staged]. *)
    let result =
      Result.try_with (fun () -> process_flags ~state:(t.init ()) argv)
    in
    (fun () ->
      let result =
        result
        >>= fun (state,anon_args) ->
        Result.try_with (fun () -> t.final state anon_args)
        >>| fun args ->
        Exn.catch_and_print_backtrace ~exit (fun () -> t.main args)
      in
      match result with
      | Ok x -> x
      | Error exn ->
        let end_with_error msg =
          Printf.eprintf "Error: %s\n%s\n%!" msg (help ~cmd t);
          exit 1
        in
        match exn with
        | Help -> print_string (help ~cmd t); exit 0
        | Failure s -> end_with_error s
        | _ -> end_with_error (Exn.to_string exn)
    )
  ;;

  let get_flag_names t = List.map t.flags ~f:Flag.name

  let autocomplete t = t.autocomplete
end

module Implicit = struct
  (* What I really want to say here is
       [type t = exists 'a 'b. ('a, 'b) Explicit.t]
     Ocaml doesn't support this (although Ron says they may soon), so
     we instead build a little "object" that gathers up all observations
     of the underlying [Explicit.t] value that we use later. *)
  type t = {
    run : (allow_unknown_flags:bool
           -> allow_underscores:bool
           -> cmd:string
           -> argv:string list
           -> exit:(int -> unit)
           -> unit
           -> unit);
    help : cmd:string -> string;
    summary : unit -> string;
    get_flag_names : unit -> string list;
    usage_arg : unit -> string;
    autocomplete : unit -> Autocomplete_.t option;
    add_flags : unit Flag.t list -> t;
  }
  let run t = t.run
  let help t = t.help
  let summary t = t.summary ()
  let get_flag_names t = t.get_flag_names ()
  let usage_arg t = t.usage_arg ()
  let autocomplete t = t.autocomplete ()
  let add_flags t ~flags = t.add_flags flags
  let rec create x = {
    run =
      (fun ~allow_unknown_flags ~allow_underscores ~cmd ~argv ~exit ->
        Explicit.run x ~allow_unknown_flags ~allow_underscores ~cmd ~argv ~exit);
    help = (fun ~cmd -> Explicit.help x ~cmd);
    summary = (fun () -> Explicit.summary x);
    get_flag_names = (fun () -> Explicit.get_flag_names x);
    usage_arg = (fun () -> Explicit.usage_arg x);
    autocomplete = (fun () -> Explicit.autocomplete x);
    add_flags = (fun flags ->
      let flags = List.map flags ~f:(fun flag ->
        Flag.lift flag ~project:(fun x -> (),(fun () -> x))
      )
      in
      create {x with Explicit.flags = x.Explicit.flags @ flags}
    );
  }
end

type t =
  | Base of Implicit.t
  | Group of string * t String.Table.t (* summary, subcommands *)

let create ?autocomplete ~summary ~usage_arg ~init ~flags ~final main =
  Base
    (Implicit.create
      { Explicit.
        summary   = summary;
        usage_arg = usage_arg;
        init = init;
        autocomplete = autocomplete;
        flags = flags;
        final = final;
        main = main;
      })
;;

let create0 ?autocomplete ~summary ~usage_arg ~init ~flags ~final main =
  let final accum anonargs =
    match anonargs with
    | [] -> final accum
    | _::_ as lst ->
      printf "Error: expected 0 anonymous arguments, got %i\n%!" (List.length lst);
      exit 1
  in
  create ?autocomplete ~summary ~usage_arg ~init ~flags ~final main
;;

let create_no_accum ?autocomplete ~summary ~usage_arg ~flags ~final main =
  let init () = () in
  let final _ anonargs = final anonargs in
  create ?autocomplete ~summary ~usage_arg ~init ~flags ~final main
;;

let create_no_accum0 ?autocomplete ~summary ~usage_arg ~flags main =
  let init () = () in
  let final _ = () in
  create0 ?autocomplete ~summary ~usage_arg ~init ~flags ~final main
;;

module Autocomplete = struct
  
  let bash_autocomplete_function =
    let fname = sprintf "_jsautocom_%d" (Unix.getpid ()) in
    sprintf
"%s () {
  COMPREPLY=()
  LENV=\"\"
  i=0;
  for e in \"${COMP_WORDS[@]}\"; do
    LENV=\"$LENV COMMAND_COMP_WORDS_${i}=\\\"$e\\\"\";
    (( i++ ));
  done;
  CC=\"eval COMMAND_COMP_CWORD=\"${COMP_CWORD}\" COMMAND_COMP_POINT=\"${COMP_POINT}\" \
  $LENV ${COMP_WORDS}\"
  matching_options=`$CC`
  eval ${matching_options}
  return 0
}
complete -F %s %s" fname fname Sys.argv.(0)

  let output_bash_function () =
    print_endline bash_autocomplete_function;
    ()

  include Autocomplete_

  let print_list strings =
    let options = List.filter ~f:((<>) "") strings in
    let arr = Array.of_list options in
    Array.iteri arr ~f:(fun i contents ->
      printf "COMPREPLY[%i]='%s'; " (i + 1) contents)

  let get_environment_numeric environment =
    match Sys.getenv environment with
    | Some word ->
        if word = "" then 0
        else int_of_string word
    | None -> assert false

  let current_word () =
    get_environment_numeric "COMMAND_COMP_CWORD"

  let current_point () =
    get_environment_numeric "COMMAND_COMP_POINT" - String.length Sys.argv.(0)

  let external_completion ~autocomplete ~key ~command_line =
    let completion_line =
      match autocomplete with
      | None -> Autocomplete_.bash [`File] [key]
      | Some f -> f command_line
    in
    print_list completion_line

  let filter_matching_prefixes_and_print
      ~autocomplete ~key ~command_line strings =
    match List.filter strings ~f:(String.is_prefix ~prefix:key) with
    | [] -> external_completion ~autocomplete ~key ~command_line
    | lst -> print_list lst

  let rec autocomplete t command_line =
    match t with
    | Base implicit ->
      let flags = Implicit.get_flag_names implicit in
      let autocomplete = Implicit.autocomplete implicit in
      (match List.rev command_line with
      | [] -> print_list flags
      | key :: _ ->
          if key = "" || key.[0] <> '-' then
            external_completion ~autocomplete ~key ~command_line
          else filter_matching_prefixes_and_print ~autocomplete ~key ~command_line flags)
    | Group (_, tbl) ->
      match command_line with
      | [key] ->
        Hashtbl.keys tbl
        |! filter_matching_prefixes_and_print ~autocomplete:None ~key ~command_line
      | [] -> (* We are at the root and all the options are requested *)
          Hashtbl.keys tbl |! print_list
      | key :: argv ->
        match Hashtbl.find tbl key with
        | None -> ()
        | Some t -> autocomplete t argv

  let rec truncate_command_line ~current_word ~current_point command_line =
    (* Printf.fprintf stderr "Word: %d, Point: %d" current_word current_point; *)
    if current_word = 1 then begin
      match command_line with
      | h :: _ ->
          let current_point =
            (* We might be off due to the spaces ... is there a better way? *)
            min (String.length h) (max 0 current_point)
          in
          [String.sub h ~pos:0 ~len:current_point]
      | [] -> [""]
    end else begin
      assert (current_word > 0);
      match command_line with
      | h :: command_line ->
          let len = String.length h in
          let current_word = pred current_word in
          let current_point = current_point - (len + 1) in
          h :: truncate_command_line ~current_word ~current_point command_line
      | [] -> []
    end

  let autocomplete t command_line =
    let current_word = current_word () in
    let current_point = current_point () in
    let command_line = truncate_command_line ~current_word ~current_point command_line in
    autocomplete t command_line

  (* We clear the setting of environment variable associated with command-line
     completion so that subprocesses don't see them. *)
  let getenv_and_clear var =
    let value = Sys.getenv var in
    if is_some value then Unix.unsetenv var;
    value
  ;;

  let rec execution_mode' index =
    match getenv_and_clear ("COMMAND_COMP_WORDS_" ^ string_of_int index) with
    | None -> []
    | Some command -> command :: execution_mode' (index + 1)
  ;;

  let execution_mode () =
    match getenv_and_clear "COMMAND_OUTPUT_INSTALLATION_BASH" with
    | Some _ -> `print_bash_autocomplete_function
    | None ->
      let command_list = execution_mode' 0 in
      match command_list with
      | [] -> `run_main
      | _ :: partial_command_line -> `doing_auto_completion partial_command_line
  ;;
end

let group ~summary alist =
  List.iter alist ~f:(fun (name,_) ->
    no_underscores name
  );
  match String.Table.of_alist alist with
  | `Ok tbl -> Group (summary, tbl)
  | `Duplicate_key name -> failwith ("multiple subcommands named " ^ name)
;;

let summary = function
  | Base implicit -> Implicit.summary implicit
  | Group (summary, _) -> summary
;;


let help ~cmd t =
  match t with
  | Base implicit ->
      Implicit.help ~cmd implicit
  | Group (mysummary, tbl) ->
      let alist =
        ("help", "display a help message for a given subcommand") ::
        List.map ~f:(fun (cmd, t) -> (cmd, summary t))
          (Hashtbl.to_alist tbl)
      in
      let alist =
        List.sort alist ~cmp:(fun (x,_) (y,_) -> String.compare x y)
      in
      Help_page.render
        ~summary:mysummary
        ~usage:(cmd ^ " SUBCOMMAND")
        ~choice_type:"available subcommands"
        ~choices:(Columns.sort_align alist)
;;

let help_help ~cmd subcommands =
  printf "help_help\n%!";
  let choices =
    Help_page.help_last_sort (List.sort ~cmp:String.compare subcommands)
  in
  Help_page.render
    ~summary:"more detailed help on a subcommand"
    ~usage:(cmd ^ " help SUBCOMMAND")
    ~choice_type:"available subcommands"
    ~choices
;;

let run_internal t ~allow_unknown_flags ~allow_underscores ~cmd ~argv ~exit =
  let maybe_dashify = maybe_dashify ~allow_underscores in
  let rec loop t cmd argv =
    match t with
    | Base implicit ->
      Implicit.run implicit ~allow_unknown_flags ~allow_underscores
        ~cmd ~argv ~exit
    | Group (_, tbl) ->
      let execute_group (subcmd, rest) =
        match partial_match tbl (maybe_dashify subcmd) with
        | Some t -> loop t (cmd ^ " " ^ subcmd) rest
        | None ->
          eprintf "%s" (help ~cmd t);
          failwith ("unknown subcommand " ^ subcmd ^ " for command " ^ cmd)
      in
      match argv with
      | [] ->
        (fun () ->
          eprintf "%s" (help ~cmd t);
          eprintf ("\n\nA subcommand for %s is required\n") cmd;
          exit 1)
      | ("-help" | "help" | "h" | "?" | "-?") :: rest ->
        begin match rest with
        | [] ->
          (fun () ->
            printf "%s" (help ~cmd t);
            exit 0)
        | ("help" :: _) ->
          (fun () ->
            printf "%s" (help_help ~cmd (Hashtbl.keys tbl));
            exit 0)
        | (sub_cmd :: rest) ->
          execute_group (sub_cmd, "-help" :: rest)
        end
      | subcmd :: rest -> execute_group (subcmd, rest)
  in
  loop t cmd argv
;;

(*
    The #! protocol for stand-alone scripts groups together all embedded
    flags as one.  If the first line of a #! script reads

        #! /path/to/my/command -flag1 -flag2 -flag3

    and then we call the script as

        > script arg1 arg2

    then the argument vector passed to /path/to/my/command will be

        ["-flag1 -flag2 -flag3"; "arg1"; "arg2"]

    So we need to pull apart the first argument into three.  Likely, this
    will only happen when the first argument is a flag (starts with '-')
    and contains a space.
*)
let hash_bang_expand_arg = function
  | (first :: rest) as same ->
      if String.is_prefix first ~prefix:"-" then
        String.split first ~on:' ' @ rest
      else
        same
  | other -> other
;;

module Version = struct
  let flags ~version ~build_info = [
    Flag.noarg "-version" (fun () -> print_endline version; exit 0)
      ~doc:" Print the hg revision of this build and exit";
    Flag.noarg_mut "-build-info" (fun () -> print_endline build_info; exit 0)
      ~doc:" Print build info as sexp and exit";
  ]

  let command ~version ~build_info =
    let summary = "Print version information" in
    let usage_arg = "[-version | -build-info]" in
    let init () = () in
    let flags = flags ~version ~build_info in
    let final () _anons = () in
    let main () =
      eprintf "(no option given - printing version)\n%!";
      print_endline version;
      exit 0
    in
    create ~summary ~usage_arg ~init ~flags ~final main

  let add t ~version ~build_info =
    match t with
    | Base impl -> Base (Implicit.add_flags impl ~flags:(flags ~version ~build_info))
    | Group (summary, subcommands) ->
      group ~summary (
        String.Table.to_alist subcommands @
          [("version",command ~version ~build_info)]
      )
end

let run_staged
    ?argv
    ?(allow_unknown_flags=false)
    ?(allow_underscores=true)
    ?(hash_bang_expand=false)
    ?(exit=Pervasives.exit)
    t ~version ~build_info =
  let t = Version.add t ~version ~build_info in
  match Autocomplete.execution_mode () with
  | `print_bash_autocomplete_function ->
      (fun () ->
        Autocomplete.output_bash_function ();
        exit 0)
  | `doing_auto_completion partial_command_line ->
    (fun () ->
      Autocomplete.autocomplete t partial_command_line;
      exit 0)
  | `run_main ->
    let argv = Option.value argv ~default:(Array.to_list Sys.argv) in
    match argv with
    | [] -> failwith "no command name passed in" (* I think this is impossible *)
    | cmd :: argv ->
      let cmd = Filename.basename cmd in
      let argv = if hash_bang_expand then hash_bang_expand_arg argv else argv in
      run_internal t ~allow_unknown_flags ~allow_underscores ~cmd ~argv ~exit
;;

let run ?argv ?allow_unknown_flags ?allow_underscores
    ?hash_bang_expand ?exit t ~version ~build_info =
  run_staged ?argv ?allow_unknown_flags ?allow_underscores
    ?hash_bang_expand ?exit t ~version ~build_info ()
;;

module Annotated_field = struct
  type t = {
    field_name : string;
    flag_name : string;
    doc : string;
    value : [
      | `Optional of string option
      | `Default of string
      | `Specified of string option
      | `List of string list
      (* [`Switch v] is for boolean fields with default value [v]
         where passing the flag (with no arguments) sets it to [not v] *)
      | `Switch of bool
    ]
  }

  type accum = t list ref

  module F = Fieldslib.Field

  let with_names name field ~doc ~value = { 
    field_name = field.F.name;
    flag_name = Option.value name ~default:field.F.name;
    doc = doc;
    value = value;
  }

  let required ?name t_list ~doc field =
    let t =
      with_names name field
      ~doc:(doc ^ " (required)")
      ~value:(`Specified None)
    in
    t :: t_list
  ;;

  let default ?name default to_string t_list ~doc field =
    let default_s = to_string default in
    let t =
      with_names name field
        ~doc:(sprintf "%s (default=%s)" doc default_s)
        ~value:(`Default default_s)
    in
    t :: t_list
  ;;

  let optional ?name t_list ~doc field =
    let t =
      with_names name field
        ~doc:(doc ^ " (optional)")
        ~value:(`Optional None)
    in
    t :: t_list
  ;;

  let set ?name t_list ~doc field =
    let t =
      with_names name field
        ~doc:(doc ^ " (default=false)")
        ~value:(`Switch false)
    in
    t :: t_list
  ;;

  let clear ?name t_list ~doc field =
    let t =
      with_names name field
        ~doc:(doc ^ " (default=true)")
        ~value:(`Switch true)
    in
    t :: t_list
  ;;

  let list ?name t_list ~doc field =
    let t = with_names name field ~doc ~value:(`List []) in
    t :: t_list
  ;;

  let init t_list = ref t_list

  let alter_value t_list ~name ~f =
    match List.find t_list ~f:(fun t -> t.flag_name = name) with
    | None -> t_list
    | Some t ->
        let new_t = {t with value = f t.value} in
        new_t :: List.filter t_list ~f:(fun t -> t.flag_name <> name)
  ;;

  let to_flag t =
    let err_specified_more_than_once () =
      failwithf "%s specified more than once" t.flag_name ()
    in
    let create flag_creator handler =
      let name = "-" ^ String.tr ~target:'_' ~replacement:'-' t.flag_name in
      flag_creator name ?aliases:None handler ~doc:t.doc
    in
    match t.value with
    | `Switch default -> create Flag.noarg_mut (fun accum ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Switch _ -> `Specified (Some (string_of_bool (not default)))
        | _ -> err_specified_more_than_once ()))
    | `Specified None -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Specified None -> `Specified (Some s)
        | _ -> err_specified_more_than_once ()))
    | `Optional None -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Optional None -> `Specified (Some s)
        | _ -> err_specified_more_than_once ()))
    | `Default _ -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `Default _ -> `Specified (Some s)
        | _ -> err_specified_more_than_once ()))
    | `List [] -> create Flag.arg_mut (fun accum s ->
      accum := alter_value !accum ~name:t.flag_name ~f:(function
        | `List l -> `List (s::l)
        | _ -> assert false
      ))
    | `Optional (Some _)
    | `Specified (Some _)
    | `List (_::_)
      -> failwith "did you call to_flag more than once?"
  ;;

  let get accum of_string field =
    let field_name = field.F.name in
    let err_unspecified () = failwithf "%s not specified" field_name () in
    match List.find !accum ~f:(fun t -> t.field_name = field_name) with
    | None -> err_unspecified ()
    | Some t ->
      begin match t.value with
      | `Switch b -> of_string (Bool.to_string b)
      | `Specified (Some s) | `Default s -> of_string s
      | `Optional None -> of_string "None"
      | `Optional (Some s) -> of_string ("Some " ^ s)
      | `Specified None -> err_unspecified ()
      | `List _ -> failwith "use get_list"
      end
  ;;

  let get_opt accum of_string field =
    let field_name = field.F.name in
    let err_unspecified () = failwithf "%s not specified" field_name () in
    match List.find !accum ~f:(fun t -> t.field_name = field_name) with
    | None -> err_unspecified ()
    | Some t ->
      begin match t.value with
      | `Optional x | `Specified x -> Option.map x ~f:of_string
      | _ -> invalid_arg "Annotated_field.get_opt"
      end
  ;;

  let get_list accum of_string field =
    let field_name = field.F.name in
    let err_unspecified () = failwithf "%s not specified" field_name () in
    match List.find !accum ~f:(fun t -> t.field_name = field_name) with
    | None -> err_unspecified ()
    | Some t ->
      begin match t.value with
      | `List x -> List.map x ~f:of_string
      | _ -> invalid_arg "Annotated_field.get_list"
      end
  ;;
end


module Flags_ext = struct
  type 'a setter =
  | No_arg of 'a
  | Arg of (string -> 'a)

  let is_no_arg = function No_arg _ -> true | _ -> false

  type ('a, 'flag) t = {
    flag : 'flag;
    get : unit -> 'a;
  }

  let flag t = t.flag
  let get t = t.get ()


  (* ******************************************************** *)
  (* Flags that can occur at most once                        *)

  
  let create_internal ~default ~name ~doc setter =
    let name = "-" ^ name in
    if default = None && is_no_arg setter then
      failwithf
        "Flags_ext: Flag %s is a no_arg flag without a default (doesn't make sense)"
        name ();
    let acc = ref None in
    let flag =
      let fail () = failwithf "Flag %s should only be specified once" name () in
      match setter with
      | No_arg final  ->
        Flag.noarg name ~doc (fun () ->
          match !acc with
          | None -> acc := Some final
          | Some _ -> fail ())
      | Arg of_string ->
        Flag.string name ~doc (fun str ->
          match !acc with
          | None -> acc := Some (of_string str)
          | Some _ -> fail ())
    in
    
    let get () =
      match default with
      | Some default ->  Option.value (!acc) ~default
      | None ->
        Option.value_exn_message
          (sprintf "Required argument %s was not specified" name)
          (!acc)
    in
    { flag = flag;
      get = get;
    }

  let create ?default ~name ~doc setter =
    let (default, doc) = match default with
      | None                -> None  , sprintf "%s (required)" doc
      | Some (v, to_string) ->
        Some v,
        if is_no_arg setter then doc (* corresponds to set or clear *)
        else sprintf "%s (default=%s)" doc (to_string v)
    in
    create_internal ~default ~name ~doc setter

  let create_optional ~name ~doc setter =
    let doc = sprintf "%s (optional)" doc in
    (* optional arguments have a default of None *)
    let default = Some None in
    let setter = match setter with
      | No_arg final  -> No_arg (Some final)
      | Arg of_string -> Arg (fun s-> Some (of_string s))
    in
    create_internal ~default ~name ~doc setter


  (* ************************************************************************** *)
  (* Repeatable flags                                                           *)

  let create_many ?(at_least_one=false) ~name ~doc setter =
    let name = "-" ^ name in
    let doc = sprintf "%s (%d or more)" doc (if at_least_one then 1 else 0) in
    let acc = ref [] in
    let flag =
      match setter with
      | Arg of_string ->
        Flag.string name ~doc (fun str -> acc := (of_string str) :: !acc)
      (* This case only really makes sense when [x] is [()]. *)
      | No_arg x ->
        Flag.noarg name ~doc (fun () -> acc := x :: !acc)
    in
    let get () =
      if at_least_one && !acc = [] then
        failwithf "Flag %s must be specified at least once" name ();
      !acc
    in
    { flag = flag;
      get = get;
    }


  (* ******************************************************** *)
  (* Choice (1 of n) flags that can occur atmost once         *)
  let create_choice_internal ?default spec_list =
    let acc = ref None in
    let names = List.map spec_list ~f:(fun (name, _, _) ->  name) in
    let names_string = String.concat ~sep:"," names in
    let make_flag (name, doc, setter) = match setter with
      | No_arg v ->
        Flag.noarg name ~doc (fun () ->
          match !acc with
          | Some _ -> failwithf "Only one of  %s can be specified." names_string ();
          | None -> acc := Some v)
      | Arg of_string ->
        Flag.string name ~doc (fun str ->
          match !acc with
          | Some _ -> failwithf "Only one of  %s can be specified." names_string ();
          | None -> acc := Some (of_string str))
    in
    let flags = List.map spec_list ~f:make_flag in
    
    let get () = match (!acc, default) with
      | Some v, _      -> v
      | None  , Some v -> v
      | None  , None    ->
        failwithf "At least one of %s must be specified" names_string ()
    in
    { flag = flags;
      get = get
    }

  let foreach ls ~f =
    let rec loop = function
      | [] -> []
      | [x] -> [f x ~first:false ~last:true]
      | x::xs -> f x ~first:false ~last:false :: loop xs
    in
    let loop0 = function
      | [] -> []
      | [x] -> [f x ~first:true ~last:true]
      | (x :: xs) -> f x ~first:true ~last:false :: loop xs
    in
    loop0 ls

  let make_choice_indicator_string first last =
    match (first, last) with
    | true , _     -> ""
    
    | false, false -> "(OR) "
    | false, true  -> "(OR) "

  let create_choice ?default spec_list =
    let (default, spec_list) =  match default with
      | Some (v, to_string) ->
        (Some v,
         foreach spec_list
           ~f:(fun (name, doc, setter) ~first ~last ->
             let (arg, desc) = String.lsplit2_exn doc ~on:' ' in
             let doc = sprintf "%s %s%s (default=%s)"
               arg
               (make_choice_indicator_string first last)
               desc
               (to_string v) in
             ("-" ^ name, doc, setter)))
      | None ->
        (None,
         foreach spec_list
           ~f:(fun (name, doc, setter) ~first ~last ->
             let (arg, desc) = String.lsplit2_exn doc ~on:' ' in
             let doc = sprintf "%s %s%s"
               arg
               (make_choice_indicator_string first last)
               desc
             in
             ("-" ^ name, doc, setter)))
    in
    create_choice_internal ?default spec_list

  let create_choice_optional spec_list =
    let spec_list =
      foreach spec_list
        ~f:(fun (name, doc, setter) ~first ~last ->
          let (arg, desc) = String.lsplit2_exn doc ~on:' ' in
          let doc = sprintf "%s %s%s (optional)"
            arg
            (make_choice_indicator_string first last)
            desc
          in
          let setter = match setter with
            | Arg of_string -> Arg (fun s -> Some (of_string s))
            | No_arg v -> No_arg (Some v)
          in
          ("-" ^ name, doc, setter))
    in
    create_choice_internal ~default:None spec_list

  let create_set ~name ~doc = create
    ~default:(false,string_of_bool)
    ~name ~doc
    (No_arg true)
end


module Helpers = struct
  exception Found_anonymous_arguments with sexp
  let no_anons c anons =
    match anons with
    | [] -> c
    | _  ->
      Printf.eprintf "No anonymous arguments expected\n%!";
      raise Found_anonymous_arguments
  ;;
end
