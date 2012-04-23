EXTS = $(subst ml,cmo, $(wildcard *.ml))
BLOGS=americasview babbage buttonwood charlemagne eastern-approaches freeexchange gulliver newsbook prospero schumpeter banyan democracyinamerica
#BLOGS=americasview babbage buttonwood charlemagne dailychart  eastern-approaches freeexchange gulliver newsbook prospero schumpeter banyan democracyinamerica
NOT_WORKING=
OUTPUTDIR=books
all: bogroll

bogroll: bogroll.ml
	ocamlfind ocamlopt -g -package unix,dynlink,netclient,str -linkpkg -linkall -o $@ $+
# -ccopt -static

%.cmo: %.ml
	ocamlfind ocamlc -c $+ -g  -package unix,dynlink,netclient,str 

clean:
	rm -f *.cm[io] parser.ml lexer.ml $(EXTS) bogroll

generate-books: bogroll
	./bogroll $(OUTPUTDIR) $(foreach BLOG,$(BLOGS),http://www.economist.com/blogs/$(BLOG)/atom.xml)

