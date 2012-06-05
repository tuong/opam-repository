
let ocamlmod_str = "ocamlmod";;
let ocamlmod = A ocamlmod_str;;

rule "ocamlmod: %.mod -> %.ml"
  ~prod:"%.ml"
  ~deps:["%.mod"; ocamlmod_str]
  begin
    fun env build ->
      let modfn =
        env "%.mod"
      in
      let dirname =
        Pathname.dirname modfn
      in
        depends_from_file 
          env 
          build
          ~fmod:(fun fn -> dirname/fn)
          modfn;
        Cmd(S[ocamlmod;
              P(modfn)])
  end
;;
