(* OASIS_START *)
(* DO NOT EDIT (digest: c4916ba41183eb8355bbc594a875ba97) *)
This is the README file for the ocaml-posix-resource distribution.

(C) 2009 Sylvain Le Gall

POSIX resource operations bindings

This library defines bindings to <sys/resource.h> functions: * getrlimit,
setrlimit: control maximum resource consumption * getrusage: get information
about resource utilisation * getpriority, setpriority: get or set the nice
value

All this are usually controled through shell command like \"nice\" and
\"ulimit\".

See the files INSTALL.txt for building and installation instructions. See the
file COPYING.txt for copying conditions. 

Home page: http://ocaml-posix-resource.forge.ocamlcore.org/


(* OASIS_STOP *)
