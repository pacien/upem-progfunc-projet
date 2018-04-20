RESULT = urm
LIBS = str
SOURCES = \
  common.ml \
  parser.mli parser.ml \
  instptr.mli instptr.ml \
  reg.mli reg.ml \
  urm.mli urm.ml

OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile
include $(OCAMLMAKEFILE)
