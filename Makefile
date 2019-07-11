SOURCES = game.ml
PACKS = universeJs
RESULT = game
OCAMLMAKEFILE = ~/.opam/4.04.0+type-debugger/lib/ocaml-makefile/OCamlMakefile # /home/isstaff/asai/include/OCamlMakefile
include $(OCAMLMAKEFILE)

$(RESULT).js : byte-code
	js_of_ocaml $(RESULT)
