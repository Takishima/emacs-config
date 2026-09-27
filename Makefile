EMACS ?= emacs

.PHONY: check compile packages
check:
	$(EMACS) --batch -l early-init.el -l init.el -l test/smoke.el

compile:
	$(EMACS) --batch -l early-init.el -l init.el -l test/compile.el

packages:
	$(EMACS) --batch -l early-init.el -l init.el -l test/packages.el
