build: clean
	emacs -batch -L . -f batch-byte-compile store-git-link.el

test: build
	emacs -batch -l ert -L . -l store-git-link-test.el -f ert-run-tests-batch-and-exit

clean:
	rm -f store-git-link.elc
