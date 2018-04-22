RESULT = urm
LIBS = str
PACKS = kaputt
SOURCES = \
  common.ml \
  parser.mli parser.ml \
  instptr.mli instptr.ml \
  reg.mli reg.ml \
  urm.mli urm.ml urm_test.ml \
  main.ml

OCAMLMAKEFILE = /usr/share/ocamlmakefile/OCamlMakefile
include $(OCAMLMAKEFILE)

test: nc
	./$(RESULT) run-tests

