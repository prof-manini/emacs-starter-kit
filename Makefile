# Simplify daily operations

.PHONY: all clean distclean update-elpa recompile-init

EMACS=emacs
EMAX=$(EMACS) --batch -l ~/.emacs.d/init.el

all: update-elpa recompile-init magit-next

clean:
	find . -name '*.elc' -print0 | xargs -r0 rm

distclean: clean
	rm -rf elpa/* elpa-to-submit/loaddefs.el*

update-elpa:
	$(EMAX) || $(EMAX)

recompile-init:
	$(EMAX) -f recompile-init

magit-next: elpa-to-submit/git-modes elpa-to-submit/magit
	(cd elpa-to-submit/git-modes && git pull)
	make -C elpa-to-submit/git-modes EFLAGS="-L ~/.emacs.d/elpa/dash-*" lisp
	(cd elpa-to-submit/magit && git pull)
	make -C elpa-to-submit/magit EFLAGS="-L ../git-modes -L ~/.emacs.d/elpa/dash-*" lisp

elpa-to-submit/magit:
	git clone --branch next https://github.com/magit/magit.git $@

elpa-to-submit/git-modes:
	git clone --branch next https://github.com/magit/git-modes.git $@
