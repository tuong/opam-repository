type ('a, 'b) package = Name of 'a | Name_as of 'a * 'b

type ('a, 'b) dependencies = 
  | Dep of 'a
  | Findlib of 'b
  | No_dep

type ('a, 'b, 'c) command = 
  | Already_specified
  | Cmd of 'a

type 'a choice = 
  | Or_inclusive of 'a (* From this point, every path can be considered. For n optional dependencies, this leads to 2^n situations. *)
  | Or_exclusive of 'a (* From this point, only one path is considered. *)

let map_cmd f = function
  | Already_specified -> assert false 
  | Cmd l -> Cmd (f l)

module M
  (Opam : sig 
    val prefix : string
    val lib : string -> string (* /tmp/OPAM.ROOT/3.12.1/lib *)
  end)
  = struct

  open Printf
    
  let configure = [ "./configure" ; "--prefix" ; Opam.prefix ]

  let oasis = 
    let make = List.map (fun l -> "ocaml" :: "setup.ml" :: l) in
    Cmd (make [ [ "-configure" ; "--prefix" ; Opam.prefix ]
              ; [ "-build" ]
              ; [ "-install" ] ])

  let add_cmd l = map_cmd (fun l_head -> l_head @ l)

  let make = List.map (fun l -> "make" :: l)
  let make_ = List.map (fun c -> "make" :: [c])

  module M = struct
    let i = make [ [] ; ["install"] ]

    let mmi = Cmd i
    let dot_cmmi = Cmd (["./configure"] :: i)
    let cmmi = Cmd (configure :: i) 
  end


  let _ (* WITH optional dependencies *) = 

    let _ = (* WITHOUT mandatory initial dependencies *)
      [ Name "ocamlgraph-1.8.2", 
        Or_inclusive
          (M.cmmi (** default compilation *), 
           [ Findlib ["ocamlgraph"], add_cmd (make_ [ "install-findlib" ])
           ; Dep "lablgtk2" (* needed for "lablgnomecanvas" *), fun l -> l ])
          
      ; Name "ocamlgsl-0.6.0",
        Or_exclusive
          (Cmd (make [[]]),
           [ Findlib ["gsl"], add_cmd (make_ [ "install-findlib" ])
           ; No_dep, add_cmd (make [ ["install" ; sprintf "INSTALLDIR=%s" (Opam.lib "gsl")] ]) ])
          
      ; Name "ocurl", 
        Or_exclusive
          (Cmd [],
           [ Findlib ["curl"], (fun _ -> Cmd (["./configure --with-findlib"] :: M.i))
           ; No_dep, fun _ -> M.dot_cmmi ])

      ; Name "camomile-0.8.3", (* REMARK "make install-data" can output some error messages which are not critical (as long as it exits with 0). *)
        Or_exclusive
          (M.dot_cmmi,
           [ Findlib ["camomile"], (fun l -> l)
           ; No_dep, fun l -> l ])
      ] in

    let _ = (* WITH at least dependency to findlib *)
      [ Name_as ("ocamlnet-3.5.1", ["equeue"; "netcamlbox"; "netcgi2"; "netcgi2-plex"; "netclient"; "netgssapi"; "netmulticore"; "netplex"; "netshm"; "netstring"; "netsys"; "pop"; "rpc"; "rpc-auth-local"; "rpc-generator"; "shell"; "smtp"]),
        (* TO_CONTINUE *)
        let mk = make_ [ "all" ; "opt" ; "install" ] in
        Or_inclusive
          (Cmd [["./configure" ; "-disable-pcre"]],
           [ Dep "pcre", (function Cmd [[c; "-disable-pcre"]] -> add_cmd mk (Cmd [[c]]) | _ -> assert false)
           ; No_dep, add_cmd mk ])
      ] in

    ()


  let _ (* WITHOUT optional dependencies *) = 

    let _ = (* WITHOUT mandatory initial dependencies *)
      let name n cmd = Name n, cmd in
      [ Name "findlib-1.3.1", Already_specified
      ; Name "camlzip-1.04", Cmd (make_ [ "all" ; "allopt" ; "install" ; "installopt" ])
      ; Name "lablgtk-2.14.2", Cmd (configure :: make_ [ "world" ; "install" ])
      ; name "wyrd-1.4.5" M.cmmi
      ] in

    let _ = (* WITH at least dependency to findlib *)
      let name ?(deps = []) ?remove n cmd = Name n, cmd, deps, remove in
      let name_as ?(deps = []) ?remove (n, l) cmd = Name_as (n, l), cmd, deps, remove in
      [ name "calendar-2.03.1" M.dot_cmmi
      ; name "ocamlify-0.0.1" (* no META : generate in 'bin' only *) oasis
      ; name "lacaml-7.0.0" oasis
      ; name "ocaml-glpk-0.1.6" M.mmi
      ; name "ocamlscript-2.0.2" M.mmi
      ; name "optimization1d-0.5" oasis
      ; name "react-0.9.3" oasis
      ; name "root1d-0.2" oasis
      ; name "sqlite3-ocaml-1.6.3" (Cmd (configure :: make_ [ "all" ; "install" ])) ~remove:[ "make" ; "remove" ]
      ; name "xmlm-1.1.0" oasis
      ; name "pcre-ocaml-6.2.5" M.mmi
      ; name "ANSITerminal-0.6" oasis
      ; name_as ("lablgtk-2.14.2-oasis8", ["lablgtk2"]) oasis
      ; Name_as ("cairo-0.4.1", ["cairo2"]), oasis, [Dep "lablgtk2"], None
      ; Name_as ("archimedes-0.4.13", [ "archimedes" ; "archimedes.cairo" ; "archimedes.graphics" ; "archimedes.top" ]), oasis, [Dep "cairo2"], None
      ] in

    let _ = (* WITH at least one dependency (which is not findlib) *)
      [ Name "cairo-ocaml-1.2.0", 
        Or_exclusive
          (add_cmd
             [ [ "aclocal" ; "-I" ; "support" ]
             ; [ "autoconf" ] ]
             M.dot_cmmi,
           [ Dep "lablgtk2", (fun l -> l)
           ; No_dep, fun _ -> 
             (* add the '--without-gtk' option *)
             failwith "pkg-config --libs pangocairo | tr ' ' '\n' | grep -v pthread | tr '\n' ' '" (* INCFLAGS=... *) ])       
      ] in

    ()

end
