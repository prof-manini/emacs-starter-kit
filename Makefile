# Simplify daily operations

EMACS=emacs
EMAX=$(EMACS) --batch -l ~/.emacs.d/init.el

.PHONY: all
all: magit-next update-elpa recompile-init update-and-compile-magit-next

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

MAGIT_DIR := $(abspath elpa-to-submit/magit)
GIT_MODES_DIR := $(abspath elpa-to-submit/git-modes)
DASH_DIR = $(abspath $(wildcard elpa/dash-2*))

.PHONY: magit-next
magit-next: $(GIT_MODES_DIR) $(MAGIT_DIR)

.PHONY: update-and-compile-magit-next
update-and-compile-magit-next:
	(cd $(GIT_MODES_DIR) && git pull)
	make -C $(GIT_MODES_DIR) EFLAGS="-L $(DASH_DIR)" clean lisp
	(cd $(MAGIT_DIR) && git pull)
	make -C $(MAGIT_DIR) EFLAGS="-L $(GIT_MODES_DIR) -L $(DASH_DIR)" clean lisp

$(MAGIT_DIR):
	git clone --branch next https://github.com/magit/magit.git $@

$(GIT_MODES_DIR):
	git clone --branch next https://github.com/magit/git-modes.git $@
