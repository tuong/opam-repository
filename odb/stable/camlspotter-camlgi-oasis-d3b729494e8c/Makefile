# CamlGI
# Copyright (C) 2005: Christophe TROESTLER
#	$Id: Makefile,v 1.17 2005/06/12 21:46:50 chris_77 Exp $
#
PKGNAME		= $(shell grep name META | \
			sed -e "s/.*\"\([^\"]*\)\".*/\1/")
PKGVERSION	= $(shell grep version META | \
			sed -e "s/.*\"\([^\"]*\)\".*/\1/")

SRC_WEB		= web
SF_WEB		= /home/groups/o/oc/ocaml-cgi/htdocs

include Makefile.config

OCAMLFLAGS 	= -dtypes -I +threads
OCAMLOPTFLAGS	= -p -inline 2 -I +threads
OCAMLDOCFLAGS	=
PP		= -pp "camlp4o pa_macro.cmo pr_o.cmo"

SOURCES = camlGI.ml \
    cgi_types.ml cgi_common.ml expires.ml cookie.ml sendmail.ml \
    cgi_std.ml cgi_fast.ml cgi.ml template.ml dbiPool.ml
INTERFACES = camlGI.mli
PRIVATE_INTERFACES = cgi_common.mli
DEMOS = 

DISTFILES	= README LICENSE META \
  Makefile Makefile.config $(SOURCES) $(INTERFACES) $(PRIVATE_INTERFACES)

PKG_TARBALL 	= $(PKGNAME)-$(PKGVERSION).tar.gz
ARCHIVE 	= $(shell grep "archive(byte)" META | \
			sed -e "s/.*\"\([^\"]*\)\".*/\1/")
XARCHIVE 	= $(shell grep "archive(native)" META | \
			sed -e "s/.*\"\([^\"]*\)\".*/\1/")

.PHONY: all byte opt install install-byte install-opt doc ps dist
all: byte opt
byte: $(ARCHIVE)
opt: $(XARCHIVE)
doc: html
ps: cgi.ps

.PHONY: ex examples examples-byte examples-opt
ex: examples
examples: examples-byte examples-opt
examples-byte: byte
	cd examples/; $(MAKE) byte
examples-opt: opt
	cd examples/; $(MAKE) opt

camlGI.cma: $(SOURCES) camlGI.cmi
	$(OCAMLC) $(PP) -a -o $@ $(OCAMLFLAGS) $<

camlGI.cmxa: $(SOURCES) camlGI.cmi
	$(OCAMLOPT) $(PP) -a -o $@ $(OCAMLOPTFLAGS) $<

# X # ifdef OCAMLLIBDIR
# X # OCAMLFIND_DESTDIR=-destdir $(OCAMLLIBDIR)
# X # else
# X # OCAMLFIND_DESTDIR=
# X # endif
# The install rule is separate because "ocamlfind" cannot install
# incrementally
install: byte opt
	$(OCAMLFIND) remove $(PKGNAME) || true
	$(OCAMLFIND) install $(PKGNAME) META \
	  $(ARCHIVE) $(XARCHIVE) $(ARCHIVE:.cma=.cmi) $(ARCHIVE:.cma=.mli)

install-byte: byte
	$(OCAMLFIND) remove  $(PKGNAME) || true
	$(OCAMLFIND) install $(PKGNAME) META \
	  $(ARCHIVE) $(ARCHIVE:.cma=.cmi) $(ARCHIVE:.cma=.mli)

install-opt: opt
	$(OCAMLFIND) remove  $(PKGNAME) || true
	$(OCAMLFIND) install $(PKGNAME) META \
	  $(XARCHIVE) $(ARCHIVE:.cmxa=.cmi) $(ARCHIVE:.cmxa=.mli)

install-doc: doc
	[ -d $(DOCDIR) ] || mkdir -p $(DOCDIR)
	cp html/* $(DOCDIR)

uninstall:
	$(OCAMLFIND) remove $(PKGNAME)
	$(REMOVE) -rf $(DOCDIR)

# Documentation
.PHONY: html
html: html/index.html

html/index.html: $(INTERFACES) $(INTERFACES:.mli=.cmi)
	[ -d html/ ] || mkdir html
	$(OCAMLDOC) -d html -html $(OCAMLDOCFLAGS) $(INTERFACES)

# Packaging
.PHONY: dist
dist:
	[ -d $(PKGNAME)-$(PKGVERSION) ] || mkdir $(PKGNAME)-$(PKGVERSION)
	cp --preserve -r $(DISTFILES) $(PKGNAME)-$(PKGVERSION)/
	tar --exclude "CVS" --exclude ".cvsignore" --exclude-from=.cvsignore \
	  -zcvf $(PKG_TARBALL) $(PKGNAME)-$(PKGVERSION)
	rm -rf $(PKGNAME)-$(PKGVERSION)

# Dependency graph
.PHONY: Camlgi.ps
Camlgi.ps: camlGI.ps
camlGI.ps: $(INTERFACES) $(SOURCES:.ml=.cmo)
	$(OCAMLDOC) -o cgi.dot -dot -I +threads $(INTERFACES) $(SOURCES)
	dot -Tps cgi.dot > $@
	rm -f cgi.dot

# Release a Sourceforge tarball and publish the HTML doc
-include Makefile.pub

# Caml general dependencies
.SUFFIXES: .ml .mli .cmo .cmi .cmx
.mli.cmi:
	$(OCAMLC) $(OCAMLFLAGS) -c $<
%.cmo: %.ml
	$(OCAMLC) $(OCAMLFLAGS) -c $<
%.cma: # Dependencies to be set elsewhere
	$(OCAMLC) -a -o $@ $(OCAMLFLAGS) $(filter %.cmo, $^)
.ml.cmx:
	$(OCAMLOPT) $(OCAMLOPTFLAGS) -c $<
%.cmxa: # Dependencies to be set elsewhere
	$(OCAMLOPT) -a -o $@ $(OCAMLOPTFLAGS) $(filter %.cmx, $^)

.PHONY: depend dep
dep: .depend
depend: .depend
.depend: $(wildcard *.ml) $(wildcard *.mli)
	$(OCAMLDEP) $(PP) $^ > .depend

include .depend

########################################################################

.PHONY: clean dist-clean
clean:
	rm -f *~ .*~ *.o *.a *.cm[aiox] *.cmxa *.annot *.css *.spit *.spot
	rm -f $(PKG_TARBALL) cgi.ps
	rm -rf html/
	# cd examples; $(MAKE) clean
	find . -not -name *.sh -type f -perm +u=x -exec rm -f {} \;

dist-clean: clean
	rm .depend
