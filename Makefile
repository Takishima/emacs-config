EMACS ?= emacs

.PHONY: check
check:
	$(EMACS) --batch -l .emacs -l test/smoke.el
