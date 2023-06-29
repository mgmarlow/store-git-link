EMACS := emacs -Q -batch

.PHONY: build test clean

build: clean
	$(EMACS) -L . -f batch-byte-compile store-git-link.el

test: build
	$(EMACS) -l ert -L . -l store-git-link-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f store-git-link.elc
