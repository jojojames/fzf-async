.PHONY: compile test clean

EMACS ?= emacs
# fzf-native is not on MELPA; default to a sibling checkout.  Override
# from the command line if your layout differs:
#   make test FZF_NATIVE_DIR=/path/to/fzf-native
FZF_NATIVE_DIR ?= ../fzf-native

compile:
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  -f batch-byte-compile fzf-async.el

# Loads the fzf-native dynamic module before running tests.  Existing
# tests are pure-Elisp helpers and would pass without it, but loading
# it in CI catches "missing binary / wrong arch / module load fails"
# regressions and lets future async-path tests just work.
test:
	$(EMACS) -Q --batch \
	  -L . -L $(FZF_NATIVE_DIR) \
	  -l fzf-native \
	  -f fzf-native-load-dyn \
	  -l ert \
	  -l ./fzf-async-test.el \
	  -f ert-run-tests-batch-and-exit

clean:
	rm -f *.elc
