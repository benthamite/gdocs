EMACS ?= emacs
ELPACA_DIR ?= $(HOME)/.config/emacs-profiles/8.1.0-dev/elpaca/builds

LOAD_PATH = -L . -L test \
  -L $(ELPACA_DIR)/plz \
  -L $(ELPACA_DIR)/dash \
  -L $(ELPACA_DIR)/s \
  -L $(ELPACA_DIR)/transient \
  -L $(ELPACA_DIR)/cond-let \
  -L $(ELPACA_DIR)/seq \
  -L $(ELPACA_DIR)/llama

TEST_FILES = \
  test/gdocs-test-helpers.el \
  test/gdocs-auth-test.el \
  test/gdocs-api-test.el \
  test/gdocs-convert-test.el \
  test/gdocs-diff-test.el \
  test/gdocs-sync-test.el \
  test/gdocs-merge-test.el \
  test/gdocs-test.el

LOAD_TESTS = $(foreach f,$(TEST_FILES),-l $(f))

.PHONY: test test-verbose compile clean

test:
	$(EMACS) --batch $(LOAD_PATH) -l ert $(LOAD_TESTS) \
	  -f ert-run-tests-batch-and-exit

test-verbose:
	$(EMACS) --batch $(LOAD_PATH) -l ert $(LOAD_TESTS) \
	  --eval '(ert-run-tests-batch-and-exit t)'

compile:
	$(EMACS) --batch $(LOAD_PATH) \
	  --eval '(setq byte-compile-error-on-warn t)' \
	  -f batch-byte-compile gdocs*.el

clean:
	rm -f *.elc test/*.elc
