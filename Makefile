SHELL := /bin/bash
EMACS ?= emacs
CSRC := $(shell git ls-files *.[ch])
ELSRC := $(shell git ls-files *.el)
TESTSRC := $(shell git ls-files test/*.el)

ifeq ($(shell uname -s),Darwin)
	SOEXT := .dylib
else
	SOEXT := .so
endif

vterm-module$(SOEXT): $(CSRC) CMakeLists.txt
	cmake -B build
	cmake --build build --clean-first --config Release -j $$(nproc)

.PHONY: compile
compile: vterm-module$(SOEXT)
	$(EMACS) -batch -L . -L test \
	  --eval "(setq byte-compile-error-on-warn t)" \
	  --eval "(setq package-user-dir \"$(CURDIR)/deps\")" \
	  -f package-initialize \
	  -f batch-byte-compile $(ELSRC) $(TESTSRC); \
	  (ret=$$? ; rm -f $(ELSRC:.el=.elc) $(TESTSRC:.el=.elc) && exit $$ret)

.PHONY: test
test: compile
	$(EMACS) --batch -L . -L test $(patsubst %.el,-l %,$(notdir $(TESTSRC))) -f ert-run-tests-batch

.PHONY: dist-clean
dist-clean:
	( \
	PKG_NAME=`$(EMACS) -batch -L . -l vterm-package --eval "(princ (vterm-package-name))"`; \
	rm -rf $${PKG_NAME}; \
	rm -rf $${PKG_NAME}.tar; \
	)

.PHONY: dist
dist: dist-clean
	$(EMACS) -batch -L . -l vterm-package -f vterm-package-inception
	( \
	PKG_NAME=`$(EMACS) -batch -L . -l vterm-package --eval "(princ (vterm-package-name))"`; \
	rsync -R vterm-module$(SOEXT) emacs-module.h $(ELSRC) $${PKG_NAME}; \
	tar cf $${PKG_NAME}.tar $${PKG_NAME}; \
	)

define install-recipe
	( \
	PKG_NAME=`$(EMACS) -batch -L . -l vterm-package --eval "(princ (vterm-package-name))"`; \
	$(EMACS) --batch -l package --eval "(setq package-user-dir $(1))" \
	  -f package-initialize \
	  --eval "(ignore-errors (apply (function package-delete) (alist-get (quote vterm) package-alist)))" \
	  -f package-refresh-contents \
	  --eval "(package-install-file \"$${PKG_NAME}.tar\")"; \
	PKG_DIR=`$(EMACS) -batch -l package --eval "(setq package-user-dir $(1))" -f package-initialize --eval "(princ (package-desc-dir (car (alist-get 'vterm package-alist))))"`; \
	)
	$(MAKE) dist-clean
endef

.PHONY: install-dev
install-dev: dist
	$(call install-recipe,\"$(CURDIR)/deps\")

.PHONY: install
install: dist
	$(call install-recipe,package-user-dir)
