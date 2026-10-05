include epkg.mk
epkg.mk:
	emacs --batch -l package -f package-initialize -l epkg -f epkg-copy-mk

SHELL := /bin/bash
EMACS ?= emacs
CSRC := $(shell git ls-files '*.[ch]')
ELSRC := $(shell git ls-files *.el)
TESTSRC := $(shell git ls-files test/*.el)

BEAR := $(shell command -v bear 2>/dev/null)
ifneq ($(BEAR),)
	BEAR := $(BEAR) --
endif

ifeq ($(shell uname -s),Darwin)
	SOEXT := .so
else
	SOEXT := .so
endif

EPKG_FILES := vterm-module$(SOEXT) $(ELSRC)
EPKG_MAIN := vterm.el
EPKG_TEST_EL := $(TESTSRC)

.DEFAULT_GOAL := compile

.PHONY: compile
compile: vterm-module$(SOEXT) epkg-compile

vterm-module$(SOEXT): $(CSRC) CMakeLists.txt
	cmake -B build
	$(BEAR) cmake --build build --clean-first --config Release -j8

.PHONY: test
test: libvterm-test epkg-test

.PHONY: libvterm-test libvterm-clean
libvterm-test: compile
	$(MAKE) -C libvterm-mirror test

libvterm-clean:
	$(MAKE) -C libvterm-mirror clean

.PHONY: install
install: vterm-module$(SOEXT) epkg-install
