open Sexplib.Conv

(* Abstract position interface *)
module type S = sig
  type t
  val format : Format.formatter -> t -> unit
  val top : string -> t
  val none : t
end

module None = struct
  type t = unit
  let format ppf () = Format.pp_print_string ppf "<no position>"
  let top (_ : string) = ()
  let none = ()
end

module type File = sig
  type t = {
    fname : string; (* file name *) 
    byte : int; (* in bytes from 0 *)
    line : int; (* from 1 *)
    column : int; (* in bytes from 0 *)
  } with sexp

  val top : string (* file name *) -> t
  val add_newlines : t -> int -> t
  val add_columns : t -> int -> t
  val none : t
  val format : Format.formatter -> t -> unit
  val format_detailed : Format.formatter -> t -> unit
end

module File : File = struct
  type t = {
    fname : string;
    byte : int; (* in bytes from 0 *)
    line : int; (* from 1 *)
    column : int; (* in bytes from 0 *)
  } with sexp

  let top fname = { fname = fname; byte = 0; line = 1; column = 0; } 
  let add_newlines t n = { t with byte = t.byte + n; line = t.line + n; column = 0; }
  let add_columns t n = { t with byte = t.byte + n; column = t.column + n; }
  let none = { fname = ""; byte = -1; line = -1; column = -1; }
  let format_filename ppf = function
    | "" -> ()
    | s -> Format.fprintf ppf "File %s:" s
  let format ppf t = 
    if t.byte < 0 then Format.fprintf ppf "<no location>"
    else Format.fprintf ppf "%aline %d, character %d" format_filename t.fname t.line t.column
  let format_detailed ppf t = 
    if t.byte < 0 then Format.fprintf ppf "<no location>"
    else Format.fprintf ppf "%aline %d, character %d, byte %d" format_filename t.fname t.line t.column t.byte

end

(* CR jfuruse: or Location, if we follow the OCaml tradition *)
module Region = struct
  type t = {
    start : File.t;
    end_ : File.t;
  } with sexp

  let top fname = { start = File.top fname; end_ = File.top fname }
  let none = { start = File.none; end_ = File.none }
  let format ppf t = 
    if t.start.File.byte < 0 then Format.fprintf ppf "<no location>"
    else 
      let diff = t.end_.File.byte - t.start.File.byte in
      Format.fprintf ppf "line %d, character %d-%d" t.start.File.line t.start.File.column (t.start.File.column + diff)

end

