# Simplify daily operations

.PHONY: all recompile-init

EMACS=emacs
EMAX=$(EMACS) --batch -l ~/.emacs.d/init.el

all: elpa/archives/elpa/archive-contents recompile-init

recompile-init:
	$(EMAX) -f recompile-init

elpa/archives/elpa/archive-contents:
	$(EMAX)
