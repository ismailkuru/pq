SHELL:=bash

opt:=ocamlfind ocamlopt -noassert -thread -package tjr_pq

all:
	$(opt) -linkpkg -o sender shared.ml sender.ml 
	$(opt) -linkpkg -o receiver shared.ml receiver.ml

clean:
	rm -f *.{cmi,cmo,cmx,o,a,cmxa,cma} sender receiver
