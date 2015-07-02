# Simplify daily operations

EMACS=emacs
EMAX=$(EMACS) --batch -l ~/.emacs.d/init.el

.PHONY: all
all: magit update-elpa recompile-init

.PHONY: clean
clean:
	find . -name '*.elc' -print0 | xargs -r0 rm

.PHONY: distclean
distclean: clean
	rm -rf elpa/* elpa-to-submit/loaddefs.el*

.PHONY: update-elpa
update-elpa:
	$(EMAX) || $(EMAX)

.PHONY: recompile-init
recompile-init:
	$(EMAX) -f esk/recompile-init

.PHONY: upgrade
upgrade: upgrade-elpa upgrade-and-compile-magit

.PHONY: upgrade-elpa
upgrade-elpa:
	$(EMAX) -f esk/upgrade-packages

MAGIT_DIR := $(abspath elpa-to-submit/magit)
GIT_MODES_DIR := $(abspath elpa-to-submit/git-modes)
DASH_DIR = $(abspath $(wildcard elpa/dash-2*))

.PHONY: magit
magit: $(GIT_MODES_DIR) $(MAGIT_DIR)

$(MAGIT_DIR):
	git clone https://github.com/magit/magit.git $@

$(GIT_MODES_DIR):
	git clone https://github.com/magit/git-modes.git $@

.PHONY: upgrade-and-compile-magit
upgrade-and-compile-magit:
	(cd $(GIT_MODES_DIR) && git pull)
	make -C $(GIT_MODES_DIR) EFLAGS="-L $(DASH_DIR)" clean lisp
	(cd $(MAGIT_DIR) && git pull)
	make -C $(MAGIT_DIR) EFLAGS="-L $(GIT_MODES_DIR) -L $(DASH_DIR)" clean lisp
