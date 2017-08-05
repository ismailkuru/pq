SHELL:=bash

libname:=tjr_pq
mls:=pq_pervasive.ml pq_connection.ml pq.ml
pkg:=-thread -package unix,threads


all:
	ocamlfind ocamlc $(pkg) -c $(mls)
	ocamlfind ocamlopt $(pkg) -c $(mls)
	ocamlfind ocamlc -g -a -o $(libname).cma $(patsubst %.ml,%.cmo, $(mls)) # FIXME cmis?
	ocamlfind ocamlopt -g -a -o $(libname).cmxa $(patsubst %.ml,%.cmx, $(mls))
	-ocamlfind remove $(libname)
	ocamlfind install $(libname) META *.cmi *.o *.a *.cma *.cmxa *.cmo *.cmx 
	$(MAKE) -C bin

clean:
	rm -f *.{cmi,cmo,cmx,o,a,cmxa,cma}
	$(MAKE) -C bin clean

