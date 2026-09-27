EMACS ?= emacs

.PHONY: check compile
check:
	$(EMACS) --batch -l early-init.el -l init.el -l test/smoke.el

compile:
	$(EMACS) --batch -l early-init.el -l init.el -l test/compile.el
