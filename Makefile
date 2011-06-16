EXTS = $(subst ml,cmo, $(wildcard *.ml))
BLOGS=americasview babbage buttonwood charlemagne dailychart  eastern-approaches freeexchange gulliver newsbook prospero schumpeter
NOT_WORKING=banyan democracyinamerica
all: bogroll $(EXTS)

bogroll: bogroll.ml
	ocamlfind ocamlc -g -package unix,dynlink,xml-light,netclient -linkpkg -linkall -o $@ $+

clean:
	rm -f *.cm[io] parser.ml lexer.ml $(EXTS)

generate-books: bogroll
	./bogroll books $(foreach BLOG,$(BLOGS),http://www.economist.com/blogs/$(BLOG)/atom.xml)

