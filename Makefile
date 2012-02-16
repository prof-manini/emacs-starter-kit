# Simplify daily operations

.PHONY: all clean distclean update-elpa recompile-init

EMACS=emacs
EMAX=$(EMACS) --batch -l ~/.emacs.d/init.el

all: update-elpa recompile-init

clean:
	find . -name '*.elc' -print0 | xargs --null rm

distclean: clean
	rm -rf elpa/* loaddefs.el

update-elpa:
	$(EMAX) || $(EMAX)

recompile-init:
	$(EMAX) -f recompile-init
