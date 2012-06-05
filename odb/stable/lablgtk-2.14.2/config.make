# -*- makefile -*-

datarootdir = ${prefix}/share
CAMLC=ocamlc.opt
CAMLOPT=ocamlopt.opt
CAMLRUN=ocamlrun
CAMLDEP=ocamldep
OCAMLDOC=ocamldoc
CAMLMKTOP=ocamlmktop
CAMLMKLIB=ocamlmklib
CAMLP4O=camlp4o
CAMLBEST=opt
CAMLWIN32=no
CAMLDEP=ocamldep
CAMLLEX=ocamllex
CAMLYACC=ocamlyacc
EXE=

USE_GL=
USE_GLADE=
USE_RSVG=1
HAVE_SVGZ=
USE_GNOMECANVAS=1
USE_GNOMEUI=
USE_PANEL=
USE_GTKSPELL=
USE_GTKSOURCEVIEW=
USE_GTKSOURCEVIEW2=
USE_GTKQUARTZ=
USE_CC=
DEBUG=1
CC=gcc
RANLIB=ranlib
XA=.a
XB=
XE=
XO=.o
XS=.so
TOOLCHAIN=unix
LIBDIR=/usr/local/lib/ocaml
THREADS_LIB=system
HAS_DLL_SUPPORT=yes
HAS_NATIVE_DYNLINK=yes

# if using ocaml >= 3.08, add a -D OCAML_308 (for camlp4)
ODOC_DEF=-D OCAML_308

# if using ocaml >= 3.11, add a -D HAS_PRINTEXC_BACKTRACE (for camlp4)
HAS_PRINTEXC_BACKTRACE=-D HAS_PRINTEXC_BACKTRACE

# where to install the binaries
prefix=/usr/local
exec_prefix=${prefix}
BINDIR=$(DESTDIR)${exec_prefix}/bin

# where to install the man page
MANDIR=$(DESTDIR)${datarootdir}/man

INSTALLDIR=$(DESTDIR)/usr/local/lib/ocaml/lablgtk2
DLLDIR=$(DESTDIR)/usr/local/lib/ocaml/stublibs
LABLGLDIR=

FILT = -Wl,--export-dynamic
clean_libs = $(subst -pthread,-ldopt -pthread -ccopt -pthread,$(subst --rpath,-rpath,$(filter-out $(FILT),$(1))))

GTKCFLAGS=-pthread -I/usr/include/gtk-2.0 -I/usr/lib/x86_64-linux-gnu/gtk-2.0/include -I/usr/include/atk-1.0 -I/usr/include/cairo -I/usr/include/gdk-pixbuf-2.0 -I/usr/include/pango-1.0 -I/usr/include/gio-unix-2.0/ -I/usr/include/glib-2.0 -I/usr/lib/x86_64-linux-gnu/glib-2.0/include -I/usr/include/pixman-1 -I/usr/include/freetype2 -I/usr/include/libpng12 -I/usr/include/librsvg-2.0 -I/usr/include/gail-1.0 -I/usr/include/libgnomecanvas-2.0 -I/usr/include/libart-2.0  
GTK_LIBS = -pthread -lgtk-x11-2.0 -lgdk-x11-2.0 -latk-1.0 -lgio-2.0 -lpangoft2-1.0 -lpangocairo-1.0 -lgdk_pixbuf-2.0 -lcairo -lpango-1.0 -lfreetype -lfontconfig -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lrt -lglib-2.0  
GTKLIBS:=$(call clean_libs,$(GTK_LIBS))
GTKGL_LIBS = 
GTKGLLIBS:=$(call clean_libs,$(GTKGL_LIBS))
GLADE_LIBS = 
GLADELIBS:=$(call clean_libs,$(GLADE_LIBS))
RSVG_LIBS = -pthread -lrsvg-2 -lm -lgio-2.0 -lgdk_pixbuf-2.0 -lcairo -lgmodule-2.0 -lgobject-2.0 -lgthread-2.0 -lrt -lglib-2.0  
RSVGLIBS:=$(call clean_libs,$(RSVG_LIBS))
GNOMECANVAS_LIBS = -pthread -lgnomecanvas-2 -lart_lgpl_2 -lgtk-x11-2.0 -lgdk-x11-2.0 -latk-1.0 -lgio-2.0 -lpangoft2-1.0 -lpangocairo-1.0 -lgdk_pixbuf-2.0 -lcairo -lpango-1.0 -lfreetype -lfontconfig -lgobject-2.0 -lgmodule-2.0 -lgthread-2.0 -lrt -lglib-2.0  
GNOMECANVASLIBS:=$(call clean_libs,$(GNOMECANVAS_LIBS))
GNOMEUI_LIBS = 
GNOMEUILIBS:=$(call clean_libs,$(GNOMEUI_LIBS))
PANEL_LIBS = 
PANELLIBS:=$(call clean_libs,$(PANEL_LIBS))
GTKSPELL_LIBS = 
GTKSPELLLIBS:=$(call clean_libs,$(GTKSPELL_LIBS))
GTKSOURCEVIEW_LIBS = 
GTKSOURCEVIEWLIBS:=$(call clean_libs,$(GTKSOURCEVIEW_LIBS))
GTKSOURCEVIEWCFLAGS= 
GTKSOURCEVIEW2_LIBS = 
GTKSOURCEVIEW2LIBS:=$(call clean_libs,$(GTKSOURCEVIEW2_LIBS))
