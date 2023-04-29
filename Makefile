test:
	emacs -batch -l ert -l store-git-link.el -l store-git-link-test.el -f ert-run-tests-batch-and-exit
