SHELL=bash

CAMLC=ocamlc # ~/ocamlc
CAMLDEP=ocamldep # ~/ocamldep

LIBS=str unix threads
CAMLCINCLUDES=-I +threads $(LIBS:=.cma)

MLS:=$(wildcard *.ml)
CMOS:=$(MLS:.ml=.cmo)

# following is to force a re-eval of $(MLS)
all: 
	make before_all
	make depend
	make cmos sender.byte receiver.byte

before_all: FORCE
	find ../src \( -type f -or -type l \) -maxdepth 1 -exec ln -sf \{\} . \;

# this always gets rebuilt - depend is never present
depend:
	$(CAMLDEP) *.ml *.mli >.depend

-include .depend

# an explicit target because we need to refer to it in all target, before $(CMOS) can be determined
cmos: $(CMOS)
	touch cmos

# a problem with the following is that the cmo files have to be in an
# order that respects .depend

COMMON_CMO=common.cmo encode2.cmo file.cmo message.cmo queue3.cmo

sender.byte: $(CMOS)
	$(CAMLC) -o $@ $(CAMLCINCLUDES) $(COMMON_CMO) sender.cmo

receiver.byte: $(CMOS)
	$(CAMLC) -o $@ $(CAMLCINCLUDES) $(COMMON_CMO) receiver.cmo


########################################################################
# Default rules
.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(CAMLC) $(COMPFLAGS) $(CAMLCINCLUDES) -c $<

.mli.cmi:
	$(CAMLC) $(COMPFLAGS) $(CAMLCINCLUDES) -c $<

########################################################################
clean: FORCE
	find . ! \( -name Makefile -or -name . -or -name Makefile.order \) -exec rm -f \{\} \;

FORCE:
