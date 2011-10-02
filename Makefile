EXTS = $(subst ml,cmo, $(wildcard *.ml))
BLOGS=americasview babbage buttonwood charlemagne dailychart  eastern-approaches freeexchange gulliver newsbook prospero schumpeter banyan democracyinamerica
NOT_WORKING=
all: bogroll

bogroll: bogroll.ml
	ocamlfind ocamlopt -g -package unix,dynlink,xml-light,netclient,str -linkpkg -linkall -o $@ $+
# -ccopt -static

%.cmo: %.ml
	ocamlfind ocamlc -c $+ -g  -package unix,dynlink,xml-light,netclient,str 

clean:
	rm -f *.cm[io] parser.ml lexer.ml $(EXTS) bogroll

generate-books: bogroll
	./bogroll books $(foreach BLOG,$(BLOGS),http://www.economist.com/blogs/$(BLOG)/atom.xml)

