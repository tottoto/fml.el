.PHONY: test
test:
	emacs --batch -L . -l fml-test.el -f ert-run-tests-batch-and-exit
