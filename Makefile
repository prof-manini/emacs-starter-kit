# Simplify daily operations

.PHONY: all clean distclean update-elpa recompile-init

EMACS=emacs
EMAX=$(EMACS) --batch -l ~/.emacs.d/init.el

all: update-elpa recompile-init

clean:
	find . -name '*.elc' -print0 | xargs -r0 rm

distclean: clean
	rm -rf elpa/* elpa-to-submit/loaddefs.el*

update-elpa:
	$(EMAX) || $(EMAX)

recompile-init:
	$(EMAX) -f recompile-init
