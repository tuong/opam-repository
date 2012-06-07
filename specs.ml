type 'a package = Name of 'a

type ('a, 'b) dependencies = 
  | Dep of 'a
  | No_dep

type 'a command = 
  | Already_specified
  | Cmd of 'a

type 'a choice = 
  | Or_inclusive of 'a (* n optional dependencies lead to 2^n situations *)
  | Or_exclusive of 'a (* only one solution from this set must be taken *)

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
    
  let configure = Printf.sprintf "./configure --prefix %s" Opam.prefix

  let oasis = 
    Cmd [ sprintf "ocaml setup.ml -configure --prefix %s" Opam.prefix
        ; "ocaml setup.ml -build"
        ; "ocaml setup.ml -install" ]

  let add_cmd l = map_cmd (fun l_head -> l_head @ l)

  let mmi = Cmd [ "make" ; "make install" ]


  let _ (* optional dependencies that can be activated or not *) = 
    [ Name "ocamlgraph-1.8.2", 
      Or_inclusive
        (Cmd [ configure ; "make" ; "make install" ] (** default compilation *), 
         [ Dep "findlib", add_cmd [ "make install-findlib" ]
         ; Dep "lablgtk2" (* needed for "lablgnomecanvas" *), fun l -> l ])

    ; Name "ocamlgsl-0.6.0",
      Or_exclusive
        (Cmd [ "make" ],
         [ Dep "findlib", add_cmd [ "make install-findlib" ]
         ; No_dep, add_cmd [ sprintf "make install INSTALLDIR=%s" (Opam.lib "gsl") ] ])

    ; Name "ocurl", 
      Or_exclusive
        (Cmd [ "./configure" ; "make" ; "make install" ],
         [ Dep "findlib", (fun l -> l)
         ; No_dep, fun l -> l ])
      
    ]

  let _ (* findlib mandatory *) = 
    [ Name "calendar-2.03.1", Cmd [ "./configure" ; "make" ; "make install" ]
    ; Name "ocamlify-0.0.1", (* no META : generate in 'bin' only *) oasis
    ; Name "lacaml-7.0.0", oasis
    ; Name "ocaml-glpk-0.1.6", mmi
    ; Name "ocamlscript-2.0.2", mmi ]

  let _ (* packages without optional dependencies and without dependency to findlib *) = 
    [ Name "findlib-1.3.1", Already_specified
    ; Name "camlzip-1.04", Cmd [ "make all" ; "make allopt" ; "make install" ; "make installopt" ]
    ; Name "lablgtk-2.14.2", Cmd [ configure ; "make world" ; "make install" ] ]


  let _ = (* to do *)
    [ "ocamlnet-3.5.1" ]

end
