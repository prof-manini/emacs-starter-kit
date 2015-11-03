# Simplify daily operations

EMACS=emacs
BATCH=$(EMACS) --batch -Q
EDIR=~/.emacs.d
EMAX=$(BATCH) -l $(EDIR)/init.el
ELPA=$(BATCH) -l $(EDIR)/esk/elpa.el$(if $(wildcard $(EDIR)/$(USER)/elpa.el), -l $(EDIR)/$(USER)/elpa.el,)
ESKDIR=esk
ESKELS=$(wildcard $(ESKDIR)/*.el)
ETSDIR=elpa-to-submit
ETSAL=$(ETSDIR)/loaddefs.el
ETSELS=$(filter-out $(ETSAL),$(wildcard $(ETSDIR)/*.el))
USERELS=$(wildcard $(EDIR)/$(USER)/*.el) $(wildcard $(EDIR)/$(USER)/gnus/.gnus.el)
ALLELS=$(ESKELS) $(ETSELS) $(USERELS) $(wildcard *.el) $(wildcard overrides/*.el)
ESKLOG=/tmp/$(USER)-emacs-starter-kit.log
BCTIMESTAMP=.byte-compile-timestamp

.PHONY: all
all: magit $(ETSAL) $(BCTIMESTAMP) update-elpa

.PHONY: clean
clean:
	rm -f $(ALLELS:.el=.elc) $(BCTIMESTAMP)

.PHONY: distclean
distclean: clean
	rm -rf elpa/* $(ETSAL)

$(ETSAL): $(ETSELS)
	@echo "Generating $@..."
	@cd $(ETSDIR) && $(BATCH) --eval "(progn\
	  (fset 'message (lambda (&rest _)))\
	  (setq make-backup-files nil)\
	  (setq vc-handled-backends nil)\
	  (setq default-directory (file-truename default-directory))\
	  (setq generated-autoload-file (expand-file-name \"loaddefs.el\"))\
	  (setq find-file-visit-truename t)\
	  (update-directory-autoloads default-directory)))" >>$(ESKLOG) 2>/dev/null

$(BCTIMESTAMP): $(ALLELS)
	@echo "Compiling $?..."
	@$(EMAX) -f batch-byte-compile $? >>$(ESKLOG) 2>&1
	@touch $@

.PHONY: update-elpa
update-elpa:
	@echo "Installing missing packages from ELPA..."
	$(ELPA) -f package-refresh-contents -f esk/install-packages

.PHONY: upgrade
upgrade: upgrade-elpa upgrade-and-compile-magit

.PHONY: upgrade-elpa
upgrade-elpa:
	$(ELPA) -f esk/upgrade-packages

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
