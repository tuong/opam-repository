# OASIS_START
# OASIS_STOP
This is a camlp4 extension that expands brace expansions like a shell
does. See sample.ml for examples.

HOW TO USE IT:

See sample.ml for explanations. See the Makefile how to compile sample.ml.

KNOWN BUGS:

Errors reported by xstrp4 are usually off by 2 (for O'Caml 3.10). This
is O'Caml bug #4357 (http://caml.inria.fr/mantis/view.php?id=4357).


CHANGES:
Changed in version 1.8:
	Removal of the support for O'Caml 3.09 and earlier.
	Integrating Sylvain's record access patch.

Changed in version 1.7:
	Porting to O'Caml 3.10. Still works for older O'Caml versions.

Changed in version 1.6:
	Setting the name of the location variable explicitly
	because the default changed in O'Caml 3.09.

Changed in version 1.5:
	Fixes for O'Caml 3.08. There is still a known problem:
	Locations in error messages may be wrong for <:here< ... >>.
	This seems to be a bug in camlp4.

Changed in version 1.4:
	Better code is generated. (Suggested by Mike Potanin.)

Changed in version 1.3:
	The backslash sequences \DDD and \xXX are recognized,
	e.g. \033 or \x21. (Suggested by Mike Potanin.)

Changed in version 1.2:
	The printf qualifiers L, l, n are now supported, e.g.
        ${x,%ld} for an int32 variable x. (Suggested by Nadj.)

Changed in version 1.0:
	Support for findlib-0.4

Changed in version 0.1.1:
	Updated the URLs in documentation.

AUTHOR:

The module has been written by Gerd Stolpmann, 
gerd@gerd-stolpmann.de

You can download it from 
http://www.ocaml-programming.de/packages/.

This module has an entry in the O'Caml link database,
http://links.camlcity.org/

Subversion:
See https://godirepo.camlcity.org/svn/
