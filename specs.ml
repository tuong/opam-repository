type ('a, 'b) package = Name of 'a | Name_as of 'a * 'b

type ('a, 'b, 'c) dependencies = 
  | Dep of 'a
  | Findlib of 'b
  | Dep_cmd_line of 'c
  | No_dep

type 'a command = 
  | Already_specified
  | Cmd of 'a

type 'a choice = 
  | Or_inclusive of 'a (* From this point, every path can be considered. For n optional dependencies, this leads to 2^n situations. *)
  | Or_exclusive of 'a (* From this point, only one path is considered. *)

type ('a, 'b) remove =
  | RM_findname
  | RM_findlib of 'a
  | RM_findlibs of 'a list
  | RM_cmd of 'b
  | RM_not_precised
  | RM_findlib_no_meta

type ('a, 'b) install = 
  | Bin of 'a
  | Bin_one of 'b

let map_cmd f = function
  | Already_specified -> assert false 
  | Cmd l -> Cmd (f l)

let cut_at_aux fn s sep =
  try
    let i = fn s sep in
    let name = String.sub s 0 i in
    let version = String.sub s (i+1) (String.length s - i - 1) in
    Some (name, version)
  with _ ->
    None

let cut_at = cut_at_aux String.index


let nv_of p = 
  let module T = Types in
        let sep = '-' in
        let n, v = 
          match 
            match
              cut_at p sep 
            with
              | Some (("ocaml"|"pcre"|"sqlite3"), _) -> cut_at_aux String.rindex p sep
              | o -> o
          with
            | None        -> 
                (match p with
                  | "ocurl" -> "ocurl", "0"
                  | "v6.0.0" -> "pa_monad_custom", "v6.0.0"
                  | "v1.0.1" -> "planck", "v1.0.1"
                  | _ ->
                      try
                        match cut_at (List.find ((=) p) ["v1.0.1" ; "v6.0.0"]) '.' with
                          | None -> failwith "none"
                          | Some (n, v) -> n, v
                      with
                        | _ -> Printf.kprintf failwith "2dozzz %S" p)
            | Some (n, v) -> n, v in
        T.NV.create (T.N.of_string n) (T.V.of_string v)
(*
                match Str.split (Str.regexp "-") p with
                    [n;v] ->
                      T.NV.create (T.N.of_string n) (T.V.of_string v)
                  | _-> assert false*)

let escape_antislash = Str.global_replace (Str.regexp_string "/") "\\/"

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

  let oasis_opt l = 
    let make = List.map (fun l -> "ocaml" :: "setup.ml" :: l) in
    Cmd (make [ ( "-configure" :: l )
              ; [ "-build" ]
              ; [ "-install" ] ])

  let oasis_opt l = 
    let make = List.map (fun l -> "ocaml" :: "setup.ml" :: l) in
    Cmd (make [ ( "-configure" :: l )
              ; [ "-build" ]
              ; [ "-install" ] ])

  let add_cmd_1 l = map_cmd (fun l_head -> l_head @ l)
  let add_cmd_2 l = map_cmd (fun l_head -> l @ l_head)

  let make = List.map (fun l -> "make" :: l)
  let make_ = List.map (fun c -> "make" :: [c])

  module M = struct
    let i = make [ [] ; ["install"] ]

    let mmi = Cmd i
    let dot_cmmi = Cmd (["./configure"] :: i)
    let cmmi = Cmd (configure :: i) 
    let configure_mmi l = Cmd (("./configure" :: l) :: i)
  end



  type cmd = string list list command
  type pkg = (string, string list) package
  type dep = (string, string * string, string) dependencies
  type rm = (string, string list) remove
  type inst = (string * string, string) install

  module To_check_later = struct

    let _ = 
      [ "bin_prot-108.00"
      ; "variantslib.108.00.opam"
      ; "ulex.1.1"
      ; "typehashlib.108.00"
      ] (* check remove ! *)
  end

  module With_opt (* WITH optional dependencies *) : sig 
    val without_mandat : 
           (pkg * (cmd * (dep * (cmd -> cmd)) list) choice * (string, string list) remove) list
(*    val with_findlib : (pkg * (cmd * (dep * (cmd -> cmd)) list) choice) list*)
  end = struct

    let without_mandat = (* WITHOUT mandatory initial dependencies *)
      let name ?rm n cmd = Name n, cmd, match rm with None -> RM_not_precised | Some rm -> rm in
      [ name "ocamlgraph-1.8.2"
          (Or_inclusive
             (M.cmmi (** default compilation *), 
              [ Findlib ("ocamlgraph", "ocamlgraph"), add_cmd_1 (make_ [ "install-findlib" ])
              ; Dep "lablgtk2" (* needed for "lablgnomecanvas" *), fun l -> l ]))
          
      ; name "ocamlgsl-0.6.0"
          (Or_exclusive
             (Cmd (make [[]]),
              [ Findlib ("ocamlgsl", "gsl"), add_cmd_1 (make_ [ "install-findlib" ])
              ; No_dep, add_cmd_1 (make [ ["install" ; sprintf "INSTALLDIR=%s" (Opam.lib "gsl")] ]) ]))
          ~rm:(RM_findlib "gsl")
             
      ; name "ocurl"
          (Or_exclusive
             (Cmd [],
              [ Findlib ("ocurl", "curl"), (fun _ -> Cmd (["./configure" ; "--with-findlib"] :: M.i))
              ; No_dep, fun _ -> M.dot_cmmi ]))
          ~rm:(RM_findlib "curl")

      ; name "camomile-0.8.3"
        (* REMARK "make install-data" can output some error messages which are not critical (as long as it exits with 0). *)
        (* REMARK The time of some files in this archive is important for 'make' *)
          (Or_exclusive
             ((let camomile = Opam.lib "camomile" in
               M.configure_mmi [ "--prefix" ; Opam.prefix
                               ; Printf.sprintf "--sbindir=%s/sbin" camomile
                               ; Printf.sprintf "--libexecdir=%s/libexec" camomile
                               ; Printf.sprintf "--sysconfdir=%s/etc" camomile
                               ; Printf.sprintf "--sharedstatedir=%s/com" camomile
                               ; Printf.sprintf "--localstatedir=%s/var" camomile
                               ; Printf.sprintf "--libdir=%s/lib" camomile
                               ; Printf.sprintf "--includedir=%s/include" camomile
                               ; Printf.sprintf "--datarootdir=%s/share" camomile ]),
              [ Findlib ("camomile", "camomile"), (fun l -> l)
              ; No_dep, fun l -> l ]))
          ~rm:RM_findname
      ]

    let with_findlib = (* WITH at least dependency to findlib *)
      [ (Name_as ("ocamlnet-3.5.1", ["equeue"; "netcamlbox"; "netcgi2"; "netcgi2-plex"; "netclient"; "netgssapi"; "netmulticore"; "netplex"; "netshm"; "netstring"; "netsys"; "pop"; "rpc"; "rpc-auth-local"; "rpc-generator"; "shell"; "smtp"]),
         (* TO_CONTINUE *)
         let mk = make_ [ "all" ; "opt" ; "install" ] in
         Or_inclusive
           (Cmd [["./configure" ; "-disable-pcre"]],
            [ Dep "pcre", (function Cmd [[c; "-disable-pcre"]] -> add_cmd_2 mk (Cmd [[c]]) | _ -> assert false)
            ; No_dep, add_cmd_2 mk ]))
      ; Name_as ("lbfgs-0.8.3", ["lbfgs"]), (* REMARK 'gfortran' is required (or 'g95', or 'g77') *)
        Or_exclusive (oasis, [ Dep "lacaml", (fun l -> l)
                             ; No_dep, fun l -> l ])
      ; Name_as ("lwt-2.3.2", [ "lwt"
                              ; "lwt.extra"
                              ; "lwt.preemptive"
                              ; "lwt.simple-top"
                              ; "lwt.syntax"
                              ; "lwt.syntax.log"
                              ; "lwt.syntax.options"
                              ; "lwt.unix"
                              ] (* WARNING all these can depend on the options described later *)), 
        Or_inclusive 
          (oasis, List.map
            (function s_option, is_enabled -> 
              Dep_cmd_line s_option, 
              let s_enabled = if is_enabled then "enable" else "disable" in
              function 
                | Cmd [ configure ; make ; make_install ] -> 
                    Cmd [ configure @ [ Printf.sprintf "--%s-%s" s_enabled s_option ] 
                        ; make
                        ; make_install ]
                | _ -> assert false)
            [ "libev", true
            ; "toplevel", false
            ; "ssl", false
            ; "react", false
            ; "text", false
            ; "unix", true
            ; "glib", false
            ; "preemptive", true
            ; "extra", true ])
      ]
  end


  module Without_opt (* WITHOUT optional dependencies *) : sig 
    val without_mandat : (pkg * cmd * inst list option) list
    val with_findlib : (pkg * cmd * dep list * rm * inst option) list 
    val to_check : (pkg * cmd * dep list * rm * inst list option) list 
    val with_onedep : (pkg * (cmd * (dep * (cmd -> cmd) * rm option) list) choice) list
  end = struct

    let without_mandat = (* WITHOUT mandatory initial dependencies *)
      let name ?install n cmd = Name n, cmd, install in
      [ name "findlib-1.3.1" Already_specified
      ; name "camlzip-1.04" (Cmd (make_ [ "all" ; "allopt" ; "install" ; "installopt" ]))
      ; name "lablgtk-2.14.2" (Cmd (configure :: make_ [ "world" ; "install" ])) ~install:[ Bin_one "src/gdk_pixbuf_mlsource"
                                                                                          ; Bin_one "src/lablgtk2" ]
      ; name "wyrd-1.4.5" (let wyrd = Opam.lib "wyrd" in
                           M.configure_mmi [ "--prefix" ; Opam.prefix
                                           ; Printf.sprintf "--sbindir=%s/sbin" wyrd
                                           ; Printf.sprintf "--libexecdir=%s/libexec" wyrd
                                           ; Printf.sprintf "--sysconfdir=%s/etc" wyrd
                                           ; Printf.sprintf "--sharedstatedir=%s/com" wyrd
                                           ; Printf.sprintf "--localstatedir=%s/var" wyrd
                                           ; Printf.sprintf "--libdir=%s/lib" wyrd
                                           ; Printf.sprintf "--includedir=%s/include" wyrd
                                           ; Printf.sprintf "--datarootdir=%s/share" wyrd ]) ~install:[Bin ("wyrd.opt", "wyrd")]
      ]

    let with_findlib = (* WITH at least dependency to findlib *)
      let name ?(deps = []) ?rm ?install n cmd = Name n, cmd, deps, (match rm with None -> RM_not_precised | Some rm -> rm), install in
      let name_as ?(deps = []) ?rm (n, l) cmd = Name_as (n, l), cmd, deps, (match rm with None -> RM_not_precised | Some rm -> rm), None in
      [ name "calendar-2.03.1" M.dot_cmmi ~rm:RM_findname
      ; name "ocamlify-0.0.1" (* no META : generate in 'bin' only *) oasis ~install:(Bin_one "_build/src/ocamlify") ~rm:(RM_findlib_no_meta)
      ; name_as ("lacaml-7.0.0", ["lacaml" ; "lacaml.top"]) oasis ~rm:RM_findname
      ; name "ocaml-glpk-0.1.6" M.mmi ~rm:(RM_findlib "glpk")
      ; name "ocamlscript-2.0.2" M.mmi ~install:(Bin_one "ocamlscript") ~rm:RM_findname
      ; name "optimization1d-0.5" oasis ~rm:RM_findname
      ; name "react-0.9.3" oasis ~rm:RM_findname
      ; name "root1d-0.2" oasis ~rm:RM_findname
      ; name "sqlite3-ocaml-1.6.3" (Cmd (configure :: make_ [ "all" ; "install" ])) ~rm:(RM_cmd [ "make" ; "remove" ]) ~install:(Bin_one "sqlite3top")
      ; name "xmlm-1.1.0" oasis ~install:(Bin ("_build/test/xmltrip.native", "xmltrip")) ~rm:RM_findname
      ; name "pcre-ocaml-6.2.5" M.mmi ~rm:(RM_findlib "pcre")
      ; name "ANSITerminal-0.6" oasis ~rm:RM_findname
      ; name_as ("lablgtk-2.14.2-oasis8", ["lablgtk2"]) oasis ~rm:(RM_findlib "lablgtk2")
      ; name_as ("cairo-0.4.1", ["cairo2"]) oasis ~deps:[Findlib ("lablgtk", "lablgtk2")] ~rm:(RM_findlib "cairo2")
      ; name_as ("archimedes-0.4.13", [ "archimedes" ; "archimedes.cairo" ; "archimedes.graphics" ; "archimedes.top" ])
        (let archimedes = Opam.lib "archimedes" in
          oasis_opt
           [ "--prefix" ; Opam.prefix
           ; Printf.sprintf "--sbindir" ; Printf.sprintf "%s/sbin" archimedes
           ; Printf.sprintf "--libexecdir" ; Printf.sprintf "%s/libexec" archimedes
           ; Printf.sprintf "--sysconfdir" ; Printf.sprintf "%s/etc" archimedes
           ; Printf.sprintf "--sharedstatedir" ; Printf.sprintf "%s/com" archimedes
           ; Printf.sprintf "--localstatedir" ; Printf.sprintf "%s/var" archimedes
           ; Printf.sprintf "--datarootdir" ; Printf.sprintf "%s/share" archimedes ])
        ~deps:[Findlib ("cairo", "cairo2")]
        ~rm:RM_findname

      ; name_as ("batteries-1.4.3", [ "batteries"
                                    ; "batteries.pa_comprehension"
                                    ; "batteries.pa_comprehension.syntax"
                                    ; "batteries.pa_llist"
                                    ; "batteries.pa_llist.syntax"
                                    ; "batteries.pa_string"
                                    ; "batteries.pa_string.syntax"
                                    ; "batteries.syntax"
                                    ; "estring" (* ? camomile ? *)
                                    ]) oasis ~deps:[Dep "camomile"] ~rm:(RM_findlibs ["estring" ; "batteries"])
      ; name_as ("CamlGI-0.6", ["CamlGI"]) M.mmi ~rm:RM_findname
      ; name_as ("extlib-1.5.2", ["extlib"]) (Cmd [["ocaml" ; "install.ml" ; "-b" ; "-n" ; "-doc"]]) ~rm:RM_findname
      ; name "ocaml-getopt-0" oasis ~rm:(RM_findlib "getopt") ~install:(Bin_one ("_build/sample"))
      ; name "odepack-0.6.2" oasis ~rm:RM_findname
      ; name "res-3.2.0" M.mmi ~rm:RM_findname
      ; name "bench-1.3" oasis ~rm:RM_findname
      ; name "v6.0.0" oasis ~rm:(RM_findlib "monad-custom")
      ; name "xstrp4-1.8" (Cmd (["./configure"] :: make_ [ "all" ; "install" ])) ~rm:RM_findname
      ; name "zarith-1.1" M.dot_cmmi ~rm:RM_findname (* WARNING findlib can be considered as optionnal *)
      ; name "type_conv-108.00" Already_specified
      ]

    let to_check = 
      let name ?(deps = []) ?rm ?install n cmd = Name n, cmd, deps, (match rm with None -> RM_not_precised | Some rm -> rm), install in
(*      let name_as ?(deps = []) ?rm (n, l) cmd = Name_as (n, l), cmd, deps, (match rm with None -> RM_not_precised | Some rm -> rm), None in*)
      [ name "ocamlmod-0.0.3" oasis ~install:[Bin ("_build/src/ocamlmod.byte", "ocamlmod")] ~deps:[Dep "fileutils"] (*~rm:(RM_findlib "monad-custom")*) 
      ; name "oasis-0.3.0" oasis ~deps:[ Dep "fileutils" ; Dep "ocaml-data-notation" ; Dep "ocamlify" ; Dep "ocamlmod" ] ~install:[Bin ("_build/src/cli/Main.byte", "oasis")] ~rm:(RM_findlibs [ "plugin-loader" ; "userconf" ; "oasis" ])
      ; name "gettext-0.3.4" (let gettext = Opam.lib "gettext" in
               M.configure_mmi [ "--disable-doc"
                               ; "--prefix" ; Opam.prefix
                               ; Printf.sprintf "--sbindir=%s/sbin" gettext
                               ; Printf.sprintf "--libexecdir=%s/libexec" gettext
                               ; Printf.sprintf "--sysconfdir=%s/etc" gettext
                               ; Printf.sprintf "--sharedstatedir=%s/com" gettext
                               ; Printf.sprintf "--localstatedir=%s/var" gettext
                               ; Printf.sprintf "--libdir=%s/lib" gettext
                               ; Printf.sprintf "--includedir=%s/include" gettext
                               ; Printf.sprintf "--datarootdir=%s/share" gettext ])
        ~deps:[ Dep "ounit" ; Dep "fileutils" ; Dep "camomile" ] 
        ~install:[Bin_one "_build/bin/ocaml-gettext" ; Bin_one "_build/bin/ocaml-xgettext"]
        ~rm:(RM_findlibs [ "gettext-camomile" ; "gettext-stub" ; "gettext" ])

      ; name "core-108.00" (let sed_i l_exp src = 
                "sed" :: "-i" ::
                  List.flatten 
                  (List.map
                     (fun x -> [ "-e" ; x ]) 
                     l_exp)
                @ [ src ] in
                     Cmd (sed_i [ Printf.sprintf "37s#%s#%s#"
                               "SRC=\"$(mktemp \"./discover_src.XXXXXXX.c\")\""
                               "SRC=\"$(mktemp -d \"./discover_src.XXXXXXX\")/c.c\""
                           ] "lib/discover.sh" :: match oasis with Cmd c -> c | Already_specified -> assert false) ) 
        ~deps:[ Dep "bin_prot"
              ; Dep "fieldslib"
              ; Dep "pa_ounit"
              ; Dep "pipebang"
              ; Dep "sexplib"
              ; Dep "variantslib"
              ; Dep "res"
              ; Dep "ounit" ]   
 ]

    let _ =
      let name ?(deps = []) ?rm ?install n cmd = Name n, cmd, deps, (match rm with None -> RM_not_precised | Some rm -> rm), install in
 [ name "core-108.00.01" (let sed_i l_exp src = 
                "sed" :: "-i" ::
                  List.flatten 
                  (List.map
                     (fun x -> [ "-e" ; x ]) 
                     l_exp)
                @ [ src ] in
                     Cmd (sed_i [ Printf.sprintf "37s#%s#%s#"
                               "SRC=\"$(mktemp \"./discover_src.XXXXXXX.c\")\""
                               "SRC=\"$(mktemp -d \"./discover_src.XXXXXXX\")/c.c\""
                           ] "lib/discover.sh" :: match oasis with Cmd c -> c | Already_specified -> assert false) ) 
        ~deps:[ Dep "bin_prot"
              ; Dep "fieldslib"
              ; Dep "pa_ounit"
              ; Dep "pipebang"
              ; Dep "sexplib"
              ; Dep "variantslib"
              ; Dep "res"
              ; Dep "ounit" ] ]

    let with_onedep = (* WITH at least one dependency (which is not findlib) *)
      [ Name "cairo-ocaml-1.2.0", 
        Or_exclusive
          (add_cmd_2
             [ [ "aclocal" ; "-I" ; "support" ]
             ; [ "autoconf" ] ]
             (*M.dot_cmmi*)
             (let cairo = Opam.lib "cairo" in
              let sed_i l_exp src = 
                "sed" :: "-i" ::
                  List.flatten 
                  (List.map
                     (fun x -> [ "-e" ; x ]) 
                     l_exp)
                @ [ src ] in
              Cmd
                ( [ "./configure"
                  ; "--prefix" ; Opam.prefix
                  ; Printf.sprintf "--sbindir=%s/sbin" cairo
                  ; Printf.sprintf "--libexecdir=%s/libexec" cairo
                  ; Printf.sprintf "--sysconfdir=%s/etc" cairo
                  ; Printf.sprintf "--sharedstatedir=%s/com" cairo
                  ; Printf.sprintf "--localstatedir=%s/var" cairo
                  ; Printf.sprintf "--libdir=%s/lib" cairo
                  ; Printf.sprintf "--includedir=%s/include" cairo
                  ; Printf.sprintf "--datarootdir=%s/share" cairo ]
                  :: sed_i
                      [ Printf.sprintf "11s#INSTALLDIR = $(OCAMLLIB)/cairo#%s#" 
                        ((*escape_antislash*) (Printf.sprintf "INSTALLDIR=%s" (Opam.lib "cairo"))) ]
                      "config.make"
                  :: sed_i
                      [ Printf.sprintf "s#$(DESTDIR)$(OCAMLLIB)/stublibs#%s#" 
                          (Printf.sprintf "%s/lib/stublibs" Opam.prefix) ]
                      "src/Makefile"
                  :: M.i )),
           [ Dep "lablgtk", 
             (fun l -> l), 
             Some (RM_cmd ("rm" :: List.map (Printf.sprintf "%s/lib/stublibs/%s" Opam.prefix)
                              [ "dllmlcairo_lablgtk.so" ; "dllmlcairo.so" ; "dllmlpangocairo.so" ]) )
           ; No_dep, 
             (fun _ -> 
               (* add the '--without-gtk' option *)
               failwith "pkg-config --libs pangocairo | tr ' ' '\n' | grep -v pthread | tr '\n' ' '" (* INCFLAGS=... *)),
             None ]
           
          )
      ]

  end

end


module MM = M (struct let prefix = "%{prefix}%" let lib = Printf.sprintf "%%{lib}%%/%s" end)
