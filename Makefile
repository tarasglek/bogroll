EXTS = $(subst ml,cmo, $(wildcard *.ml))

all: bogroll $(EXTS)

bogroll: bogroll.ml
	ocamlfind ocamlc -g -package unix,dynlink,xml-light,netclient -linkpkg -linkall -o $@ $+


clean:
	rm -f *.cm[io] parser.ml lexer.ml $(EXTS)
