EMACS ?= emacs

.PHONY: check
check:
	$(EMACS) --batch -l early-init.el -l init.el -l test/smoke.el
